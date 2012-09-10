(*Maya Saietz
* IP uge 2
* Individuel aflevering *)

(*2I1*)
(*brydcaesar : int * int -> int
* Finder nøglen til et caesar-chiffer ud fra et stykke
* klartekst k og et stykke chiffer-tekst c.
* Antager at input er korrekt, altså at der findes en
* nøgle.*)
fun brydcaesar (k,c) = ((c mod 10) - (k mod 10)) mod 10

(*212*)
val testbrydcaesar = brydcaesar (72926349, 27471894) = 5

(*213*)
(*Det virker ikke fordi input ikke passer sammen -
* der findes ikke en nøgle som passer.
* Jeg spørger om den er lig med 5, fordi det er det jeg
* forventer, ikke fordi det er korrekt.*)
val testbrydcaesarForkert = brydcaesar (72926349, 27461894) = 5
