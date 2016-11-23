module Cat

import World

%access export

record Cat where
  constructor MkCat
  catHealth : Int
  catBlack  : Bool

catAttack : Int -> Cat -> Cat
catAttack x (MkCat h b) = if b then MkCat (h + x) b
                               else if h <= x then MkCat 0 True
                                              else MkCat (h - x) b
