module Impredicative.Physics (inspectWorld) where

import Impredicative.World

inspectWorld :: IO String
inspectWorld = foldWorld f
  where f r e = "(id: " ++ show r ++ ", health: " ++ show (health e) ++ ") "