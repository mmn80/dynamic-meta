module Main (main) where

import World
import Physics
import Cat

inspect :: IO ()
inspect = putStr "world state: " >> inspectWorld >>= putStrLn

main :: IO ()
main = do
  putStrLn "spawning cat..."
  k <- spawn $ Cat 100 False
  inspect
  putStrLn "attacking cat..."
  _ <- withEntity k (\e -> let e' = attack 5 e in (e', health e'))
  inspect
