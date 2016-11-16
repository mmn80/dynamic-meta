module Existential.Cat where

import Existential.World

data Cat = Cat { catHealth :: Int, blackCat :: Bool }

instance Entity Cat where
  health = catHealth
  attack p (Cat h b) = if b then Cat (h + p) b
                            else if p > h then Cat 0 True
                                          else Cat (h - p) b
