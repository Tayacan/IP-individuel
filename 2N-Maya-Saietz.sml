(*Maya Saietz
* Nød, uge 2
* *)

local
  (*factors : int -> int list
  * Lists all the factors of an integer x > 0*)
  fun factors x = List.filter (fn i => x mod i = 0) 
                              (List.tabulate (x div 2, fn i => i+1))
in
  (*sigma : int -> int
  * Multiplies all the factors of a number n > 0*)
  fun sigma n = foldl op+ 0 (factors n)
end

val testSigma1 = sigma 12 = 16
val testSigma2 = sigma 220 = 284 

(*amicablep : int -> int -> bool
* Test if two numbers are amicable*)
fun amicablep x y = x = sigma y andalso y = sigma x

val testAmicable1 = not (amicablep 27 29)
val testAmicable2 = amicablep 220 284

(*perfectp : int -> bool
* Test if a number is perfect*)
fun perfectp n = sigma n = n

val testPerfect1 = not (perfectp 30)
val testPerfect2 = perfectp 6

(* amicableNumbers : int * int list -> int list
* Returnerer listen af venskabstal under n*)
fun amicableNumbers (0,found) = found
  | amicableNumbers (n,found)  = 
let
  infix isIn;
  fun _ isIn [] = false
    | x isIn (y::ys) = if x = y then true else x isIn ys
in
  if n isIn found then amicableNumbers (n-1,found)
  else if perfectp n then amicableNumbers (n-1,n::found)
  else if amicablep n (sigma n) then amicableNumbers (n-1,n::(sigma n)::found)
  else amicableNumbers(n-1,found)
end
