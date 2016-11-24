module Physics

import World

%access export
%default total

inspectWorld : Host -> String
inspectWorld w = foldWorld w $ \_, i, e => "(health: " ++ show (health i e) ++ ") "

attack1 : Host -> Host
attack1 w = fst $ withEntity w $ \_, i, e => (attack i 1 e, health i e)
