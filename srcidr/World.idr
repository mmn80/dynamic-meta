module World

%access export

record Entity e where
  constructor MkEntity
  _entity : e
  _health : e -> Int
  _attack : Int -> e -> e

health : Entity e -> Int
health (MkEntity x h _) = h x

attack : Int -> Entity e -> Entity e
attack d (MkEntity x h a) = MkEntity (a d x) h a

HList : Type
HList = List (e : Type ** Entity e)

spawn : HList -> Entity e -> HList
spawn l x = (e ** x) :: l

withEntity : HList -> ((e : Type) -> Entity e -> (Entity e, b)) -> (HList, Maybe b)
withEntity [] f = ([], Nothing)
withEntity ((a ** x) :: xs) f = let (a', x) = f a x
                                in ((a ** a') :: xs, Just x)

foldWorld : Monoid m => HList -> ((e : Type) -> Entity e -> m) -> m
foldWorld w f = foldl f' neutral w
  where f' r (a ** ent) = r <+> f a ent
