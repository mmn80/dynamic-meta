module Church.Cat where

import Church.World

data Cat = Cat { catHealth :: Int, blackCat :: Bool }

spawnCat :: Cat -> IO Ref
spawnCat cat = spawn $ Entity cat catHealth catAttack

catAttack :: Int -> Cat -> Cat
catAttack p (Cat h b) = if b then Cat (h + p) b
                            else if p > h then Cat 0 True
                                          else Cat (h - p) b
