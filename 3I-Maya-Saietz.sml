(*Maya Saietz
* IP uge 3
* Individuel aflevering *)

(*3I1 {{{*)
(* repchar : 'a * int -> 'a list
 * Gentager en char n gange. Kaster Domain hvis n
 * er negativ. *)
fun repchar (_,0) = []
  | repchar (c,n) = if n < 0 then raise Domain
                             else c::repchar (c,n-1)
(*}}}*)
(*3I2 {{{*)
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
(*}}}*)
local
  (*Hjælpefunktioner{{{*)
  fun countprefix (x::xs) =
  let
    fun cp (x,[],r) = (r,[])
      | cp (x,y::xs,r) = if x = y then cp (x,xs,r+1) else (r,y::xs)
    val (count,tail) = cp(x,xs,1)
  in
    (x,count,tail)
  end
  | countprefix _ = raise Fail "Empty list"

  fun countallprefixes [] = []
    | countallprefixes xs =
    let
      val (e,n,tail) = countprefix xs
    in
      (e,n)::(countallprefixes tail)
    end

  fun rlcwrite [] = ""
    | rlcwrite ((c,n)::xs) = implode(repchar(c,n))^(rlcwrite(xs))

  fun f (a,b) = (Char.toString a)^(if b = 1 then "" else Int.toString b)

  fun g (x::xs) = " "^(f x)^(g xs)
    | g [] = ""

  fun digits x = if x < 10 then 1
                 else 1 + digits (x div 10)

  fun getN NONE = (1,0)
    | getN (SOME x) = (x,digits x)

  fun fromString s = if String.isPrefix " " s
                     then NONE
                     else Int.fromString s

  fun decompress3 [] = []
    | decompress3 [c] = [c]
    | decompress3 (#" "::x::xs) = let
                                    val (n,l) = getN (fromString (implode xs))
                                    val tail = List.drop (xs,l)
                                  in repchar (x,n) @ decompress3 tail
                                  end
    | decompress3 _ = raise Fail "Bad input"
  (*}}}*)
in
  (*3I3 {{{*)
  (* rldecompress : string -> string
   * Dekomprimerer en løbslængdekomprimeret
   * tekst. *)
  fun rldecompress s =
  let
    val cs = combine (explode s)
    fun expand [] = ""
      | expand ((c,n)::rest) = if Char.isDigit c orelse not (Char.isDigit n)
                               then raise Fail "Bad input"
                               else let
                                 (*Trækker ascii-værdier fra hinanden*)
                                 val num = (Char.ord n) - (Char.ord #"0")
                               in
                                 implode (repchar (c, num)) ^ expand rest
                               end
  in
    expand cs
  end
  (*}}}*)
  (*3I4 {{{*)
  (* rldecompress2 : string -> string
   * Som rldecompress, men kan håndtere at der
   * er flere end 9 tegn i træk, samt at der ikke
   * står et 1-tal efter et enkeltstående tegn. *)
  fun rldecompress2 s =
  let
    val cs = explode s

    fun decompress2 [] = []
      | decompress2 [c] = [c] (*antager et enkeltstående tegn*)
      | decompress2 (x::xs) = if Char.isDigit x
                              then raise Fail "Bad input"
                              else let
                                val (n,l) = getN (Int.fromString (implode xs))
                                val tail = List.drop (xs,l)
                              in repchar (x,n) @ decompress2 tail
                              end
  in
    implode (decompress2 cs)
  end
  (*}}}*)
  (*3I5 {{{*)
  (* rlcompress3 : string -> string
   * løbslængdekomprimerer en tekst, som også kan
   * indeholde tal. Indsætter mellemrum for at adskille
   * tegn.*)
  val rlcompress3 = g o countallprefixes o explode

  (* rldecompress3 : string -> string
   * dekomprimerer en tekst komprimeret af
   * rlcompress3. *)
  val rldecompress3 = implode o decompress3 o explode
  (*}}}*)
end

(*Tests{{{*)
(*Tests af 3I1 {{{*)
val testrepchar1 = repchar (#"a",0) = []
val testrepchar2 = repchar (#"b",5) = [#"b",#"b",#"b",#"b",#"b"]
val testrepchar3 = (repchar (#"c",~3);false) handle Domain => true;
(*}}}*)
(*Tests af 3I2 {{{*)
val testcombine1 = combine [] = []
val testcombine2 = (combine [1,2,3];false) handle OddList => true
val testcombine3 = combine [1,2,3,4,5,6,7,8] = [(1,2),(3,4),(5,6),(7,8)]
(*}}}*)
(*Tests af 3I3 {{{*)
val testrldecompress1 = rldecompress "a2b4c3" = "aabbbbccc"
val testrldecompress2 = (rldecompress "a34c";false)
          handle Fail "Bad input" => true
val testrldecompress3 = (rldecompress "a3b4c";false) handle OddList => true
(*}}}*)
(*Tests af 3I4 {{{*)
val testrldecompress21 = rldecompress2 "" = ""
val testrldecompress22 = rldecompress2 "a3" = "aaa"
val testrldecompress23 = rldecompress2 "abc" = "abc"
val testrldecompress24 = rldecompress2 "a12" = "aaaaaaaaaaaa"
(*}}}*)
(*Tests af 3I5 {{{*)
val testrlcompress31 = rlcompress3 "a" = " a"
val testrlcompress32 = rlcompress3 "ggggggggggg" = " g11"
val testrlcompress33 = rlcompress3 "33344" = " 33 42"
val testrldecompress31 = rldecompress3 " a" = "a"
val testrldecompress32 = rldecompress3 " g11" = "ggggggggggg"
val testrldecompress33 = rldecompress3 " 33 42" = "33344"
(*}}}*)
(*}}}*)

