(*Maya Saietz
* Nød, uge 4
* *)
let
  val q = "\""
  val reverse = foldr (fn (a,b) => a ^ "\n" ^ b) "" o
    map (implode o rev o explode) o String.tokens
    (fn c => c = #"\n")
  val escape = implode o foldr (fn (c,l) => if c = #"\\" orelse
                                               c = #"\"" orelse
                                               c = #"\n"
                                  then #"\\"::c::l
                                  else c::l) [] o explode
  val join = foldr (fn (a,b) => a ^ "\n" ^ b) ""
  val quotes = fn s => "              ,\"" ^ s ^ "\""
  fun quoteList (x::xs) = "               \"" ^
    x ^ "\""::(map quotes xs)
  val text1 = [
               "(*Maya Saietz"
              ,"* Nød, uge 4"
              ,"* *)"
              ,"let"
              ,"  val q = \"\\\"\""
              ,"  val reverse = foldr (fn (a,b) => a ^ \"\\n\" ^ b) \"\" o"
              ,"    map (implode o rev o explode) o String.tokens"
              ,"    (fn c => c = #\"\\n\")"
              ,"  val join = foldr (fn (a,b) => a ^ \"\\n\" ^ b) \"\""
              ,"  val quotes = fn s => \"              ,\\\"\" ^ s ^ \"\\\"\""
              ,"  fun quoteList (x::xs) = \"               \\\"\" ^"
              ,"    x ^ \"\\\"\"::(map quotes xs)"
              ,"  val text1 = ["
              ]
  val text2 = [
              "in"
              ,"  reverse"
              ,"  (join text1 ^ join ((quoteList o map escape) text1) ^"
              ,"  \"              ]\\n\\n  val text2 = [\\n\" ^"
              ,"  join ((quoteList o map escape) text2) ^"
              ,"  \"              \\n]\" ^ join text2)"
              ,"end"
              ]
in
  reverse
  (join text1 ^ join ((quoteList o map escape) text1) ^
  "              ]\n  val text2 = [\n" ^
  join ((quoteList o map escape) text2) ^
  "              ]\n" ^ join text2)
end
