{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Impredicative.World (Entity(..), health, attack, Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map

data Entity a = Entity a (a -> Int) (Int -> a -> a)

health :: Entity a -> Int
health (Entity e h _) = h e

attack :: Entity a -> Int -> Entity a
attack (Entity e h a) x = Entity (a x e) h a

type SomeEntity = forall x. (forall a. Entity a -> x) -> x

mkSomeEntity :: Entity a -> SomeEntity
mkSomeEntity e = \f -> f e

withSomeEntity :: forall b. SomeEntity -> (forall e. Entity e -> (Entity e, b)) -> (SomeEntity, b)
withSomeEntity se f = se $ \e -> let (e', b) = f e
                                 in (mkSomeEntity e', b)

world :: MVar (Map.IntMap SomeEntity)
world = unsafePerformIO $ newMVar (Map.empty :: Map.IntMap SomeEntity)

newtype Ref = Ref { getRef :: Int } deriving Num

instance Show Ref where show (Ref k) = show k

ref :: MVar Ref
ref = unsafePerformIO . newMVar $ Ref 0

spawn :: Entity a -> IO Ref
spawn e = do
  Ref k <- takeMVar ref
  let r = Ref (k + 1)
  putMVar ref (r `seq` r)
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (mkSomeEntity e) w
  return r

withEntity :: Ref -> (forall a. Entity a -> (Entity a, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just e -> let (e', x) = withSomeEntity e f in
                                 (Map.insert k e' w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: forall m. Monoid m => (forall a. Ref -> Entity a -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' :: Int -> SomeEntity -> m
        f' k se = snd $ withSomeEntity se $ \e -> (e, f (Ref k) e)