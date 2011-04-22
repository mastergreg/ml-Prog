fun len nil = 0
|   len (h::t) = 1 + len t;
fun lentr l =
  let 
    fun hel [] i = i
    |   hel (h::t) i = hel t (i+1)
  in hel l 0
  end



exception HDempty;
fun hg [] = raise HDempty
|   hg (h::t) = h




fun split (x::y::tl) =
    let
      val (s,t) = split(tl)
    in (x::s,y::t)
    end
|   split l = (l,[])


