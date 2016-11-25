module Physics

import World

%access export

inspectWorld : Host -> String
inspectWorld w = foldWorld w $ \_, i, k, e =>
  "(id: " ++ show k ++ ", health: " ++ show (health i e) ++ ");"

attack' : Int -> Key -> Host -> Host
attack' d k w = fst $ withEntity w k $ \_, i, e => (attack i d e, ())
