(*Maya Saietz
* IP uge 4
* Individuel aflevering *)

(* 4I1{{{*)
(* suffixsen : string list -> string list
 * Sætter "sen" efter hver string i en liste *)
fun suffixsen xs = map (fn s => s ^ "sen") xs

(* suffixgen : string -> string list -> string list
 * Sætter et suffix efter hver string i en liste *)
fun suffixgen suffix xs = map (fn s => s ^ suffix) xs

(* Tests *)
val testsuffixsen = suffixsen ["a","Peter","fe"] = ["asen","Petersen","fesen"]
val testsuffixsenEmpty = suffixsen [] = []

val testsuffixgen1 = suffixgen "sen" ["a","Peter","fe"] =
        suffixsen ["a","Peter","fe"]
val testsuffixgen2 = suffixgen "er" ["fisk","løb","tab"] =
        ["fisker","løber","taber"]
val testsuffixgenEmpty = suffixgen "hej" [] = []
val testsuffixgenEmpty2 = suffixgen "" ["a","b"] = ["a","b"]
(*}}}*)
(* 4I2{{{*)
(* mellemsaml : string list -> string
 * Sammensætter en liste af strings ved at sætte mellemrum ind.*)
fun mellemsaml [] = ""
  | mellemsaml [t] = t
  | mellemsaml (t::ts) = t ^ foldl (fn (s,acc) => acc ^ " " ^ s) "" ts

(* Tests *)
val testmellemsaml = mellemsaml ["Kage","er","nice"] = "Kage er nice"
val testmellemsamlEmpty = mellemsaml [] = ""

(* Den givne funktion mellemsaml' vil sætte et mellemrum til sidst. Det sker
 * fordi det første kald til foldr sætter den sidste string i listen sammen med
 * " " ^ "", altså " ".*)

(*}}}*)
(* 4I3{{{*)
(* korteLange : string list -> int * int
 * Tæller antallet af korte og lange strings i en liste, og returnerer tuplen
 * (korte,lange).
 * En string er lang hvis den har syv eller flere tegn, ellers kort.*)
val korteLange = foldl (fn (s,(k,l)) => if size s >= 7
                                        then (k,l+1)
                                        else (k+1,l)) (0,0)

(* Tests *)
val testkorteLange1 = korteLange ["kort","laaaaaang","k","lang"] = (3,1)
val testkorteLange2 = korteLange ["k","o","r","t"] = (4,0)
val testkorteLangeEmpty = korteLange [] = (0,0)
(*}}}*)
