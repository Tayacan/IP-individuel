signature MonadLib =
sig
type 'a m
val return : 'a -> 'a m
val bind : 'a m -> ('a -> 'b m) -> 'b m
val ap : ('a -> 'b) m -> 'a m -> 'b m
val >>= : 'a m * ('a -> 'b m) -> 'b m
val <*> : ('a -> 'b) m * 'a m -> 'b m
val <$> : ('a -> 'b) * 'a m -> 'b m
end

signature Monad =
sig
    type 'a m
    val return : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
end

functor Monad(m: Monad) : MonadLib where type 'a m = 'a m.m =
struct
  type 'a m = 'a m.m
  val bind = m.bind
  val return = m.return
  infix >>=
  fun x >>= f = bind x f
  fun flip f a b = f b a
  fun opp0 f ma b = ma >>= return o (flip f b)
  fun opp f ma mb = mb >>= opp0 f ma
  fun ap0 f a = f a 
  fun ap f = opp ap0 f
  fun pure f = (return f)
  infix <*>
  fun f <*> a = ap f a
  infix <$>
  fun f <$> a = ap (pure f) a
end
