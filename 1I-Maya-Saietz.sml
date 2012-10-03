(*Maya Saietz
* IP uge 1
* Individuel aflevering *)

load "Bool";

(*1I1*)
(*compareReal : real * real -> bool
* sammenligner to reals*)
fun compareReal (a,b) = 
let
  val epsilon = 0.000000001
in
  abs(a-b) < epsilon
end;

(*powerRealInt : real * int -> real*)
fun powerRealInt (_, 0) = 1.0
  | powerRealInt (r, n) = if n < 0
                                 then 1.0 / powerRealInt (r, ~ n)
                                 else r * powerRealInt (r, n - 1);

val test1I1 = compareReal(powerRealInt (0.99, ~100),2.731999026);

(*1I2*)
(*relativePrimes : int * int -> bool
* Hvis den største fælles divisor er 1,
* er 1 også den eneste fælles divisor *)
fun relativePrimes (a,b) = 
let
  fun gcd (0,n) = n
    | gcd (m,n) = gcd (n mod m, m)
in
  gcd (a,b) = 1
end;

val test1I2_a = relativePrimes (6,7);
val test1I2_b = not (relativePrimes (6,8));

(*1I3*)
(*nextNotRelativePrime : int -> int*)
fun nextNotRelativePrime n = 
let
  fun nextNRP (a,b) = if relativePrimes (a,b)
                      then nextNRP (a, b+1)
                      else b
in
  nextNRP (n,n+1)
end;

val test1I3 = nextNotRelativePrime 119 = 126;
val test1I3b = nextNotRelativePrime 7 = 14; 
