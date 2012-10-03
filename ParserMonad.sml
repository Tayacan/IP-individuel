;use "Monad.sml";

infixr $
fun a $ b = a b
datatype 'a parser = Parser of (string -> ('a * string) list)
fun parse (Parser p) = p

structure ParserMonad : Monad =
struct
  type 'a m = 'a parser
  fun return x = Parser $ (fn cs => [(x,cs)])
  fun bind p q = Parser $ (fn cs => let
                             fun mparse p' (a,b) = parse (p' a) b
                             val l1 = parse p cs
                             val l2 = List.concat $ map (mparse q) l1
                           in
                             l2
                           end)
end

structure Parser = Monad(ParserMonad)

open Parser;

val reject = Parser (fn _ => [])

val item = let fun item' [] = []
                 | item' (x::xs) = [(x,implode xs)]
           in  Parser $ item' o explode
           end

fun charp x = Parser $ (fn s => let val ((c,cs)::_) = parse item s
                                in if c = x then [(c,cs)]
                                            else []
                                end)
