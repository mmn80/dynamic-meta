module Cat

import World

%access export
%default total

record Cat where
  constructor MkCat
  catHealth : Int
  catBlack  : Bool

%name Cat cat, cat'

catAttack : Int -> Cat -> Cat
catAttack x (MkCat h b) = if b then MkCat (h + x) b
                               else if h <= x then MkCat 0 True
                                              else MkCat (h - x) b

entityCat : Entity Cat
entityCat = MkEntity catHealth catAttack

spawnCat : Cat -> Host -> Host
spawnCat = spawn entityCat
