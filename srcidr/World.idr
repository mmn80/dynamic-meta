module World

%access export
%default total

public export
record Entity t where
  constructor MkEntity
  health : t -> Int
  attack : Int -> t -> t

%name Entity i, i', i''

Host : Type
Host = List (t : Type ** (Entity t, t))

%name Host w, w', w''

spawn : Entity t -> t -> Host -> Host
spawn i e w = (t ** (i, e)) :: w

withEntity : Host -> ((t : Type) -> Entity t -> t -> (t, b)) -> (Host, Maybe b)
withEntity [] _ = ([], Nothing)
withEntity ((t' ** (i, e)) :: w) f = let (e', x) = f t' i e
                                     in ((t' ** (i, e')) :: w, Just x)

foldWorld : Monoid m => Host -> ((t : Type) -> Entity t -> t -> m) -> m
foldWorld w f = foldl f' neutral w
  where f' m (t ** (i, e)) = m <+> f t i e
