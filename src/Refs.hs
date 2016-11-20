{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Refs (Ref(..), newRef) where

import Control.Concurrent.MVar
import System.IO.Unsafe

newtype Ref = Ref { getRef :: Int } deriving Num

instance Show Ref where show (Ref k) = show k

ref :: MVar Ref
ref = unsafePerformIO . newMVar $ Ref 0

newRef :: IO Ref
newRef = do
  Ref k <- takeMVar ref
  let r = Ref (k + 1)
  putMVar ref (r `seq` r)
  return r
