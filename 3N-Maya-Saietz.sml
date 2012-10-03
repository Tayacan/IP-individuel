(*Maya Saietz
* Nød, uge 3
* *)

(*Summen af listen sammenlignet med summen af listen af elementer fra 0 til
* n-1*)

fun split ([],_,ls,gs) = (ls,gs)
  | split ((x::xs),n,ls,gs) = if x <= n then split (xs,n,x::ls,gs)
                                        else split (xs,n,ls,x::gs)
(*sum{{{*)
fun tjek [] = true
  | tjek xs =
let
  val sum = foldl op+ 0 xs
  val l = length xs

  (*sum af tal fra 0 til n*)
  fun nsum (0,n) = n
    | nsum (x,n) = nsum (x-1,x+n)

in
  (sum = nsum ((length xs)-1,0))
end
(*}}}*)
(* fjern {{{*)
(* minUdenfor : int list -> int
 * Finder det mindste ikke-negative tal som ikke er en del af en liste *)
fun minUdenfor xs =
  let
    val l = length xs (*O(n)*)
    fun fjern ((x::xs),n,min,max) =
        if x >= n
        then if x = min then fjern (xs,n-1,min+1,(if max > n-1
                                                  then max -1 else max))
                else if x = max
                     then fjern (xs,n-1,min,max-1)
                     else fjern (xs,n-1,min,(if max > n-1 then max-1 else max))
        else if x = min
             then fjern (xs,n-1,min+1,max)
             else if x = max
                  then fjern (xs,n-1,min,max-1)
                  else let val (ys,mi,ma) = fjern (xs,n,min,max)
                       in (x::ys,mi,ma)
                       end
      | fjern ([],n,min,max) = ([],min,max)

    val (it1,min1,max1) = fjern (xs,l,0,l-1)
    val (it2,min2,max2) = fjern (rev it1,length it1,min1,max1)
  in
    if tjek xs
    then l
    else max2
  end
(* }}}*)
(*argumentation {{{
(* Argumentation for at den er korrekt:
 * Først vil jeg forklare hvordan funktionen fjern virker, da den kan være en
 * smule uoverskuelig.
 * Funktionen tager som parameter en tupel indeholdende listen af elementer der
 * arbejdes på, længden af listen (n), den mindste værdi som måske kunne være
 * svaret (min), og det største element (max) hvor vi ved at svaret ikke ligger
 * mellem max og længden af den oprindelige liste.
 * n starter som længden af den oprindelige liste, og der trækkes en fra hver
 * gang et element fjernes.
 * min starter som 0, og der lægges en til hvis et element er lig med min.
 * max starter som n-1, og der trækkes en fra hvis et element er lig med max.
 * Returværdien er en tupel som indeholder hvad der er tilbage af listen efter
 * at have fjernet de elementer der skal fjernes, samt de fundne min og max
 * værdier.
 * Et element x fjernes hvis et eller flere af følgende udsagn er sande:
 * x >= n
 * x = min
 * x = max
 *
 * Hvis listen er sorteret stigende vil min altid være svaret når funktionen
 * returnerer. fx med listen [0,1,3,4] vil min først være 0, så 0 fjernes og min
 * bliver 1, derefter fjernes 1 og min bliver 2, og derfra er der ikke nogle
 * elementer som er lig med min.
 * Hvis listen er sorteret faldende, er det derimod n der skal bruges. For
 * eksempel med listen [4,3,1,0] vil n først være 4. 4 fjernes og n bliver 3, 3
 * fjernes og n bliver 2, og ingen af de resterende elementer er større end
 * eller lig med 2.
 * Nu er det desværre sådan at listen ikke er sorteret, så vi er nød til at
 * finde på noget bedre. Min første ide var at køre listen igennem
 * fjern-funktionen, vende den resulterende liste om, og køre den igennem igen.
 * Det virker dog kun for nogle lister, som for eksempel [5,1,3,0], hvor 5 og 3
 * bliver taget af sammenligningen med n, og 0 og 1 ryger i sammenligningen med
 * min. Her vil min altså være 2 når det andet kald til fjern-funktionen 
 * returnerer. Til gengæld vil funktionen slå fejl med en liste som 
 * [4,3,1,0,5,2], hvor svaret skal være 6, men min vil være 2 når funktionen
 * returnerer. Svaret er selvfølgelig at vi skal bruge den originale listes
 * længde, men hvordan kan vi vide det?
 * Det er her parameteret max kommer ind i billedet.*)

(* Argumentation for at den kører i linær tid:
 * Vi starter med at gøre nogle ting i et let-udtryk, så lad os kigge på dem
 * først. At tage længden af en liste er højest linær tid (konstant hvis SML
 * gemmer meta-data om listen, det ved jeg ikke om den gør). Funktionen fjern
 * kigger på hvert element en gang, så den er også i linær tid. Den indbyggede
 * rev-funktion er også linær.
 * Vi kalder i alt length to gange (den ene gang på en liste af samme eller
 * mindre længde), fjern to gange (igen, anden gang på en forhåbenligt kortere
 * liste), og rev en gang. Der ingen andre funktionskald. Det er altså 5 gange 
 * listens længde i værste tilfælde.*)
 }}}*)
