(*Maya Saietz
* IP uge 3
* Individuel aflevering *)

(*3I1*)
(* repchar : char * int -> char list
 * Gentager en char n gange. Kaster Domain hvis n
 * er negativ. *)
fun repchar (_,0) = []
  | repchar (c,n) = if n < 0 then raise Domain
                             else c::repchar (c,n-1)

(*Tests af 3I1*)
val testrepchar1 = repchar (#"a",0) = []
val testrepchar2 = repchar (#"b",5) = [#"b",#"b",#"b",#"b",#"b"]
val testrepchar3 = (repchar (#"c",~3);false) handle Domain => true;

(*3I2*)
(* combine : 'a list -> 'a * 'a list
 * Laver en liste om til en liste af par.
 * Kaster fejlen OddList hvis listen 
 * indeholder et ulige antal elementer.
 * Man kunne vælge at tjekke længden inden
 * man beregner, men koden er kortere og
 * pænere sådan her fordi man kan bruge
 * mønstre.*)
exception OddList;
fun combine [] = []
  | combine [x] = raise OddList
  | combine (x::y::xs) = (x,y)::combine xs

(*Tests af 3I2*)
val testcombine1 = combine [] = []
val testcombine2 = (combine [1,2,3];false) handle OddList => true
val testcombine3 = combine [1,2,3,4,5,6,7,8] = [(1,2),(3,4),(5,6),(7,8)]

(*3I3*)
(* rldecompress : string -> string
 * Dekomprimerer en løbslængdekomprimeret
 * tekst. *)
fun rldecompress s =
let
  val cs = combine (explode s)
  fun expand [] = ""
    | expand ((c,n)::rest) =
    let
      val num = (Option.valOf o Int.fromString o Char.toString) n
                        handle Option => raise Fail "Bad input"
    in
      implode (repchar (c, num)) ^ expand rest
    end
in
  expand cs
end

(*Tests af 3I3*)
val testrldecompress1 = rldecompress "a2b4c3" = "aabbbbccc"
val testrldecompress2 = (rldecompress "a34c";false)
          handle Fail "Bad input" => true
val testrldecompress3 = (rldecompress "a3b4c";false) handle OddList => true

(*3I4*)
(* rldecompress2 : string -> string
 * Som rldecompress, men kan håndtere at der
 * er flere end 9 tegn i træk, samt at der ikke
 * står et 1-tal efter et enkeltstående tegn. *)
fun rldecompress2 s =
let
  val cs = explode s
  fun dList [] = []
    | dList (x::xs) = if Char.isDigit x then x :: dList xs
                                            else []
  fun getN [] = (1,0)
    | getN xs = ((Option.valOf o Int.fromString o implode) xs ,length xs)
  fun decompress [] = []
    | decompress [c] = [c] (*antager et enkeltstående tegn*)
    | decompress (x::xs) = if Char.isDigit x
                              then raise Fail "Bad input"
                              else let val (n,l) = getN (dList xs)
                                       val tail = List.drop (xs,l)
                                   in repchar (x,n) @ decompress tail
                                   end
in
  implode (decompress cs)
end

(*Tests af 3I4*)
val testrldecompress21 = rldecompress2 "" = ""
val testrldecompress22 = rldecompress2 "a3" = "aaa"
val testrldecompress23 = rldecompress2 "abc" = "abc"
val testrldecompress24 = rldecompress2 "a12" = "aaaaaaaaaaaa"


