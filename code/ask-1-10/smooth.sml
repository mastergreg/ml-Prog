fun isprime x nil = true
|   isprime x (l::ls) = if (x mod l)=0 andalso x <> l then false else isprime x ls;

fun myrange lo hi =
  let fun r lo  = if lo > hi then [] else lo::r (lo+2)
  in r lo
  end;
fun odd_numbs 1 = [1]
|   odd_numbs n = myrange 3 n;

fun append [] ys      = ys 
|   append (x::xs) ys = x::append xs ys;

fun make_primelist nil m = m
|   make_primelist (h::t) m = if isprime h m then make_primelist t (append m [h]) else make_primelist t m;

fun so_smooth nil num nil = 1073741823
|   so_smooth nil num (h::t) = h
|   so_smooth (h::t) num smoothnes= 
        if h <= num 
          then if (num mod h) = 0 then so_smooth t num (h::smoothnes) else so_smooth t num smoothnes
          else so_smooth nil num nil;
  
fun smoothie m n = so_smooth m n nil;

fun smooth b i j =
  let 
    val primelist = make_primelist (odd_numbs j) [2]
    fun msmooth primelist b i j = 
    if i=j
    then 
      if (smoothie primelist i) <= b then 1 else 0 
    else
      if (smoothie primelist i) <= b then (1 + msmooth primelist b (i+1) j) else msmooth primelist b (i+1) j
  in msmooth primelist b i j
  end;
smooth 10 20 190000;
