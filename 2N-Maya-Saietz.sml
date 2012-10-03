(*Maya Saietz
* Nød, uge 2
* *)

(* sigma : int -> int
 * Multiplies all the factors of a number n > 0*)
fun sigma n =
let
  fun s (0,acc) = acc
    | s (x,acc) = if n mod x = 0 then s (x-1,acc+x)
                                 else s (x-1,acc)
in
  s (n div 2, 0)
end

val testSigma1 = sigma 12 = 16
val testSigma2 = sigma 220 = 284 

local
  val max = 12345
  (* sumAmicableNumbers : int * int * int -> int *)
  fun sumAmicableNumbers (0,sum) = sum
    | sumAmicableNumbers (n,sum) = 
    let
      val s = sigma n
      val toAdd = if s > max then 0 else s
    in
      if n = sigma s andalso s > n
      then sumAmicableNumbers (n-1,sum+n+toAdd)
      else sumAmicableNumbers (n-1,sum)
    end
in
  val answer = sumAmicableNumbers (12345,0)
end
