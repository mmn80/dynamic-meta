module World

import public Data.SortedMap

%access export

public export
record Entity t where
  constructor MkEntity
  health : t -> Int
  attack : Int -> t -> t

%name Entity i, i', i''

public export
Key : Type
Key = Int

public export
Host : Type
Host = SortedMap Key (t : Type ** (Entity t, t))

%name Host w, w', w''

spawn : Entity t -> Key -> t -> Host -> Host
spawn i k e w = insert k (t ** (i, e)) w

withEntity : Host -> Key -> ((t : Type) -> Entity t -> t -> (t, b)) -> (Host, Maybe b)
withEntity w k f = case lookup k w of
    Nothing             => (w, Nothing)
    Just (t' ** (i, e)) => let (e', x) = f t' i e
                           in (insert k (t' ** (i, e')) w, Just x)

foldWorld : Monoid m => Host -> ((t : Type) -> Entity t -> Key -> t -> m) -> m
foldWorld w f = foldl f' neutral (toList w)
  where f' m (k, (t ** (i, e))) = m <+> f t i k e
