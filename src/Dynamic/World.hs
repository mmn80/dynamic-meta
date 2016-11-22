{-# LANGUAGE ScopedTypeVariables #-}

module Dynamic.World (Entity(..), SomeEntity, health, attack, Ref, spawn, withEntity, foldWorld) where

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.IntMap.Strict as Map
import Data.Dynamic
import Refs

class Typeable e => Entity e where
  _health :: e -> Int
  _attack :: Int -> e -> e

-- SomeEntity e (e -> Int) (Int -> e -> e)

data SomeEntity = SomeEntity Dynamic Dynamic Dynamic

health :: SomeEntity -> Int
health (SomeEntity e h _) = fromDyn (dynApp h e) (-1)

attack :: Int -> SomeEntity -> SomeEntity
attack x (SomeEntity e h a) = SomeEntity (dynApp (dynApp a (toDyn x)) e) h a

mkSomeEntity :: forall e. Entity e => e -> SomeEntity
mkSomeEntity e = SomeEntity (toDyn e) (toDyn (_health :: e -> Int))
                                      (toDyn (_attack :: Int -> e -> e))

-- world

world :: MVar (Map.IntMap SomeEntity)
world = unsafePerformIO $ newMVar Map.empty

spawn :: Entity e => e -> IO Ref
spawn e = do
  r <- newRef
  w <- takeMVar world
  putMVar world $ Map.insert (getRef r) (mkSomeEntity e) w
  return r

withEntity :: Ref -> (SomeEntity -> (SomeEntity, b)) -> IO (Maybe b)
withEntity (Ref k) f = do
  w <- takeMVar world
  let (w', mb) = case Map.lookup k w of
                   Just e  -> let (e', x) = f e
                              in (Map.insert k e' w, Just x)
                   Nothing -> (w, Nothing)
  putMVar world w'
  return mb

foldWorld :: Monoid m => (Ref -> SomeEntity -> m) -> IO m
foldWorld f = readMVar world >>= return . Map.foldMapWithKey f'
  where f' k e = f (Ref k) e