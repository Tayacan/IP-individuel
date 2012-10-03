(*Maya Saietz
* IP uge 2
* Individuel aflevering *)

(*2I1*)
(*brydcaesar : int * int -> int
* Finder nøglen til et caesar-chiffer ud fra et stykke
* klartekst k og et stykke chiffer-tekst c.
* Antager at input er korrekt, altså at der findes en
* nøgle.*)
fun brydcaesar (k,c) = 
let
  val key = ((c mod 10) - (k mod 10)) mod 10

  (*test : int * int -> bool
  * Test om input giver mening - hvis k er klartest.
  * for c, er nøglen også korrekt.*)
  fun test (0,0) = true
    | test (0,_) = false (*Forskelligt antal cifre er forkert*)
    | test (_,0) = false 
    | test (a,b) = if (b - a) mod 10 <> key
                   then false
                   else test (a div 10,b div 10)
in
  if test (k,c) then key else raise Fail "Input giver ikke mening"
end

(*212*)
val testbrydcaesar = brydcaesar (72926349, 27471894) = 5

(*213*)
(*Det virker ikke fordi input ikke passer sammen -
* der findes ikke en nøgle som passer, fordi c ikke er
* k krypteret.*)
val testbrydcaesarForkert = (brydcaesar (72926349, 27461894);false)
                        handle Fail _ => true

(*Flere tests*)
(*base case*)
val testBaseCase = brydcaesar (0,0)
(*c er k krypteret med nøgle 1, men mangler et ciffer*)
val testAntalCifre = (brydcaesar(12345678,2345678);false)
                        handle Fail _ => true
(*Omvendt for en god ordens skyld*)
val testAntalCifre2 = (brydcaesar(2345678,12345678);false)
                        handle Fail _ => true
