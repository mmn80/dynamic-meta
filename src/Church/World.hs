{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Church.World (Entity(..), health, attack, Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map
import Refs

-- abstract Entity (hand made typeclass)

data Entity e = Entity e (e -> Int) (Int -> e -> e)

health :: forall e. Entity e -> Int
health (Entity e h _) = h e

attack :: forall e. Entity e -> Int -> Entity e
attack (Entity e h a) x = Entity (a x e) h a

-- existential wrapper around Entity

type SomeEntity = forall x. (forall e. Entity e -> x) -> x

mkSomeEntity :: forall e. Entity e -> SomeEntity
mkSomeEntity e = \f -> f e

withSomeEntity :: forall b. SomeEntity -> (forall e. Entity e -> (Entity e, b))
                                       -> (SomeEntity, b)
withSomeEntity se f = se $ \e -> let (e', b) = f e
                                 in (mkSomeEntity e', b)

-- world

world :: MVar (Map.IntMap SomeEntity)
world = unsafePerformIO $ newMVar (Map.empty :: Map.IntMap SomeEntity)

spawn :: forall e. Entity e -> IO Ref
spawn e = do
  r <- newRef
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (mkSomeEntity e) w
  return r

withEntity :: forall b. Ref -> (forall e. Entity e -> (Entity e, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just se -> let (se', x) = withSomeEntity se f
                              in (Map.insert k se' w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: forall m. Monoid m => (forall e. Ref -> Entity e -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' :: Int -> SomeEntity -> m
        f' k se = snd $ withSomeEntity se $ \e -> (e, f (Ref k) e)