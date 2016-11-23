module Physics

import World

%access export

inspectWorld : HList -> String
inspectWorld l = foldWorld l $ \_, e => "(health: " ++ show (health e) ++ ") "
