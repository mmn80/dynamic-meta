module Dynamic.Cat where

import Dynamic.World

data Cat = Cat { catHealth :: Int, blackCat :: Bool }

instance Entity Cat where
  _health = catHealth
  _attack p (Cat h b) = if b then Cat (h + p) b
                            else if p > h then Cat 0 True
                                          else Cat (h - p) b
