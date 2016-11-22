module GADT.Physics (inspectWorld) where

import GADT.World

inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id: " ++ show r ++ ", health: " ++ show (health e) ++ ") "