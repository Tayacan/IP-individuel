(* Maya Saietz
 * IP uge 5
 * Individuel aflevering *)

infixr $
fun a $ b = a b

datatype koen = Mand | Kvinde | Andet
type navn = string option
datatype aarstal = Aar of int | Ukendt | Irrelevant
datatype person  = Person of navn * koen * aarstal * aarstal

datatype 'a binTree = Node of 'a binTree * 'a * 'a binTree | Leaf
type aneTrae = person binTree

datatype foraelder = MOR | FAR

(* Træ til at teste på*)
local
  (* Personer i traet*)
  val root = Person (SOME "Bo Bosen",Mand,Aar 2010,Irrelevant)
  val mother = Person (SOME "Bodil Bosen",Kvinde,Aar 1983,Irrelevant)
  val father = Person (SOME "Bob Bosen",Mand,Aar 1979,Irrelevant)
  val mormor = Person (SOME "Boa Bosdatter",Kvinde,Aar 1959,Irrelevant)
  val morfar = Person (SOME "Bobby Bosdatter",Mand,Aar 1945,Aar 2013)
  val farmor = Person (SOME "Borghild Bosen",Kvinde,Aar 1954,Irrelevant)
  val farfar = Person (SOME "Bohr Bosen",Mand,Aar 1957,Aar 1992)
  val oldefar = Person (SOME "Bo Jr. Bosen",Mand,Ukendt,Aar 2009)
  val tipoldefar = Person (SOME "Bo",Mand,Aar 1824,Irrelevant)

  (* 0. generation *)
  val tipoldet = Node (Leaf,tipoldefar,Leaf)
  (* 1. generation *)
  val oldet = Node (Leaf,oldefar,tipoldet)
  (* 2. generation *)
  val farfart = Node (Leaf,farfar,oldet)
  val farmort = Node (Leaf,farmor,Leaf)
  val morfart = Node (Leaf,morfar,Leaf)
  val mormort = Node (Leaf,mormor,Leaf)
  (* 3. generation *)
  val fathert = Node (farmort,father,farfart)
  val mothert = Node (mormort,mother,morfart)
  (* 4. generation *)
  val roott = Node (mothert,root,fathert)
in
  val trae = roott
end

(*5I1*)
(* perslig : person * person -> bool
 * Sammenligner to personer.
 * = virker fint på personer. Hvis man vil ikke kan lide at
 * funktionen er polymorfisk, kan man tvinge typen. *)
fun perslig (a : person,b) = a = b

(*5I2*)
(* fjern : aneTrae * foraelder list -> aneTrae
 * Fjerner en person og alle dens aner fra et aneTrae.
 * Antager at mødre står til venstre, og fædre til 
 * højre. 
 * Hvis positionen ikke findes i træet, returneres
 * det gamle træ.*)
fun fjern t [] = Leaf
  | fjern (Node (m,b,f)) (MOR::ps) = Node(fjern m ps,b,f)
  | fjern (Node (m,b,f)) (FAR::ps) = Node(m,b,fjern f ps)
  | fjern Leaf           _         = Leaf

(*5I3*)

