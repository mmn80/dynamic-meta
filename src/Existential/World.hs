{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Existential.World (Entity(..), Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map
import Refs

class Entity a where
  health :: a -> Int
  attack :: Int -> a -> a

data SomeEntity = forall a. Entity a => SomeEntity a

world :: MVar (Map.IntMap SomeEntity)
world = unsafePerformIO $ newMVar Map.empty

spawn :: Entity a => a -> IO Ref
spawn e = do
  r <- newRef
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (SomeEntity e) w
  return r

withEntity :: Ref -> (forall a. Entity a => a -> (a, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just (SomeEntity e) -> let (e', x) = f e in
                                          (Map.insert k (SomeEntity e') w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: Monoid m => (forall a. Entity a => Ref -> a -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' k (SomeEntity e) = f (Ref k) e