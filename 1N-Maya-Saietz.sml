(*Maya Saietz
* Nød, uge 1*)

load "Char";
load "Int";

(*1N1*)
fun talord x =
let
  (*Liste over ord vi kan sætte sammen*)
  val wordMap = [(100,"hundrede")
                ,(90,"halvfems")
                ,(80,"firs")
                ,(70,"halvfjers")
                ,(60,"tres")
                ,(50,"halvtres")
                ,(40,"fyrre")
                ,(30,"tredive")
                ,(20,"tyve")
                ,(19,"nitten")
                ,(18,"atten")
                ,(17,"sytten")
                ,(16,"seksten")
                ,(15,"femten")
                ,(14,"fjorten")
                ,(13,"tretten")
                ,(12,"tolv")
                ,(11,"elleve")
                ,(10,"ti")
                ,(9,"ni")
                ,(8,"otte")
                ,(7,"syv")
                ,(6,"seks")
                ,(5,"fem")
                ,(4,"fire")
                ,(3,"tre")
                ,(2,"to")
                ,(1,"et")
                ,(0,"nul")]

  (*Find the word corresponding to n in the wordmap*)
  fun getWord n [] = ""
    | getWord n ((x,w)::xs) = if n = x
                              then w
                              else getWord n xs;

  fun getHundreds n = if n div 100 > 0
                      then (getWord (n div 100) wordMap) ^
                           (getWord 100 wordMap)
                      else ""

  fun getTens 0 = ""
    | getTens n = getWord ((n div 10) * 10) wordMap

  fun getOnes n = (getWord (n mod 10) wordMap)

  fun getOnesNoZero n = if n mod 10 = 0
                        then ""
                        else getOnes n ^ "og"

  fun getLessThan20 n = (getWord (n mod 20) wordMap)

  val tens = if (x mod 100) < 20
             then getLessThan20 x
             else (getOnesNoZero x) ^ (getTens (x mod 100))
in
  (*Support for negative tal, fordi det er nemt*)
  if x < 0
  then "minus " ^ talord (~ x)
  else getHundreds x ^ tens
end;

(*1N2*)
(*Kan i teorien hardcodes, da den altid bare vil returnere det samme:*)
fun findsand_snyd () = "Denne sætning indeholder præcis syvogfyrre bogstaver.";

(*Men for ikke at være usportslig, har jeg også lavet en funktion
* der beregner det. Bemærk at længden på strengen ikke passer
* hvis man tester med size funktionen, fordi æ læses som to tegn.*)
fun findsand () =
let
  val minLength = 37
  fun test n = minLength + ((length o explode o talord) n) = n
  fun findN x = if test x then x else findN (x + 1)
in
  "Denne sætning indeholder præcis " ^ (talord (findN minLength)) 
        ^ " bogstaver."
end;

(*1N3*)
(*Ligesom i 1N2 er der muligheden for at hardcode resultatet.*)
fun findsand2_snyd () = "Denne sætning indeholder præcis seksogtyve vokaler" ^ 
  " og enogfyrre konsonanter.";

(*Eller man kan beregne det*)
fun findsand2 () = 
let
  val minVowels = 18
  val minConsonants = 30
  fun getCharsInWord w chars = length (List.filter
    (fn x => List.exists (fn y => x = y) (explode chars))
    (map Char.toLower (explode w)))
  val vowels = "aeiouyæøå"
  val consonants = "bcdfghjklmnpqrstvxz"
  
  fun getVowels word = getCharsInWord word vowels
  fun getConsonants word = getCharsInWord word consonants

  (*Checker om et resultat er sandt*)
  fun test nv nk = minVowels + (getVowels (talord nv)) + 
        (getVowels (talord nk)) = nv
    andalso minConsonants + (getConsonants (talord nv)) +
        (getConsonants (talord nk)) = nk
  (*Brute force - afprøver løsninger 1 ad gangen
  * Hvis der ikke findes et resultat, sætter den
  * nuller ind, hvilket er lidt dumt*)
  fun findN 1000 1000 = (0,0)
    | findN nv 1000 = findN (nv + 1) minConsonants
    | findN nv nk = if test nv nk then (nv,nk) else findN nv (nk + 1)
  val pair = findN minVowels minConsonants
in
  "Denne sætning indeholder præcis " ^ (talord (#1 pair)) ^ " vokaler og " ^
  (talord (#2 pair)) ^ " konsonanter."
end;

(*1N4*)
(*I teorien kan denne her også hardcodes, men det kræver at man kender svaret -
* hvis der da findes et!*)

datatype Graph = nodeData of char * int list
               | node of Graph * (Graph list)

fun findsand3 () =
let
  val alphabet = explode "adefhijklmnorstuvy"
  fun getNumberOfLetters _ [] = 0
    | getNumberOfLetters letter (c::cs) = if letter = Char.toLower c
                                          then 1 + getNumberOfLetters letter cs
                                          else getNumberOfLetters letter cs

  fun getEffect (c,e,50) = e
    | getEffect (c,e,n) = getEffect (c, e + getNumberOfLetters c 
                                        (explode (talord n)), n+1)

  val effectList = map (fn c => (c,getEffect (c,0,1))) alphabet
  fun zipWithF _ [] _ = []
    | zipWithF _ _ [] = []
    | zipWithF f (x::xs) (y::ys) = f (x,y) :: zipWithF f xs ys

  val baseList = (map (fn c => (c,(getNumberOfLetters c
        (explode "Denne sætning indeholder præcis"))+1)) alphabet)

  exception Zip
  val startList = zipWithF (fn ((c1,n1),(c2,n2)) => if c1 <> c2 
                                                    then raise Zip
                                                    else (c1,n1+(n2 div 2)))
                                                        effectList baseList

  (*Sammensæt en streng ud fra en liste af (bogstav,antal) tupler*)
  fun makeString [] = "Det ved jeg ikke!"
    | makeString [(c,n)] = (talord n) ^ " " ^ (Char.toString c) ^ 
        (if n > 1 then "'er." else ".")
    | makeString ((c,n)::rest) = (talord n) ^ " " ^ (Char.toString c) ^
        (if n > 1 then "'er og "else " og ") ^ makeString rest
  (*Sæt et præfix på*)
  fun makeStringFinished [] = makeString []
    | makeStringFinished cs = "Denne sætning indeholder præcis " ^ makeString cs

  (* Check if a sentence is correct *)
  fun correct _ [] = 0
    | correct s ((c,n)::cs) = if getNumberOfLetters c s = n
                              then 1 + correct s cs
                              else correct s cs
in
  correct (explode (makeStringFinished baseList)) baseList
end

fun findsand3_inefficient (lettersToCount, maxCount) =
let
  (*
  (*Listen her skal være start-parameteret til en rekursiv funktion*)
  val minLetters =
    [(#"a",1),(#"d",4),(#"e",34),(#"f",1),(#"g",2),(#"h",2),(#"i",4)
    ,(#"j",1),(#"k",1),(#"l",2),(#"m",1)]
    *)

  val alphabet = List.take ((explode "adefhijklmnorstuvy"), lettersToCount)
  val goal = length alphabet (*For comparing to the result of test*)
  fun getNumberOfLetters _ [] = 0
    | getNumberOfLetters letter (c::cs) = if letter = Char.toLower c 
                                          then 1 + getNumberOfLetters letter cs
                                          else getNumberOfLetters letter cs

  (*utility stuff*)
  exception Letters
  fun incLetterList [] = raise Letters
    | incLetterList ((c,n)::rest) = if n = maxCount
                                    then (c,0)::incLetterList rest
                                    else (c,n+1)::rest

  (*Antallet af tegn for hvilke strengen er sand*)
  fun test s [] = 0
    | test s ((letter,number)::rest) = if getNumberOfLetters letter (explode s) = number
                              then 1 + test s rest
                              else test s rest

  (*Sammensæt en streng ud fra en liste af (bogstav,antal) tupler*)
  fun makeString [] = "Det ved jeg ikke!"
    | makeString [(c,n)] = (talord n) ^ " " ^ (Char.toString c) ^ 
        (if n > 1 then "'er." else ".")
    | makeString ((c,n)::rest) = (talord n) ^ " " ^ (Char.toString c) ^
        (if n > 1 then "'er og "else " og ") ^ makeString rest
  (*Sæt et præfix på*)
  fun makeStringFinished [] = makeString []
    | makeStringFinished cs = "Denne sætning indeholder præcis " ^ makeString cs

  fun findLetters letterList = 
        if test (makeStringFinished letterList) letterList = goal
        then letterList
        else findLetters (incLetterList letterList)
        handle Letters => []
in
  makeStringFinished (findLetters (map (fn c => (c,(getNumberOfLetters c
        (explode "Denne sætning indeholder præcis"))+1)) alphabet))
end;
