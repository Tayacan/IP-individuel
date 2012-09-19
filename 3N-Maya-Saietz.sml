(*Maya Saietz
* Nød, uge 3
* *)

fun minUdenfor xs =
let
  fun split (x,(a,b)) = [(a,x),(x,b)]

  infix isIn
  fun x isIn (a,b) = x < b andalso x > a

  fun newRanges (n,[],y) = [(0,n),(n,y-1)]
    | newRanges (n,(range::ranges),y) = if n isIn range
                                      then (split (n,range)) @ ranges
                                      else range::newRanges (n,ranges,y)

 fun findRanges (ranges,n,[]) = (ranges,n)
   | findRanges (ranges,n,(y::ys)) = if y > n
                                      then findRanges (ranges,y+1,ys)
                                      else findRanges (newRanges (y,ranges,n),n,ys)
in
  findRanges ([],~1,xs)
end
