fun isprime x nil = true
|   isprime x (l::ls) = if (x mod l)=0 andalso x <> l then false else isprime x ls;




fun myrange lo hi =
  let fun r lo  = if lo > hi then [] else lo::r (lo+2)
  in r lo
  end;
fun odd_numbs n = myrange 3 n;

fun append [] ys      = ys 
|   append (x::xs) ys = x::append xs ys;





fun make_primelist nil m = m
|   make_primelist (h::t) m = if isprime h m then make_primelist t (append m [h]) else make_primelist t m;


fun so_smooth nil num nil = num
|   so_smooth nil num h = hd(h)
|   so_smooth (h::t) num smoothnes= 
        if (num mod h) = 0 then so_smooth t num (h::smoothnes) else so_smooth t num smoothnes;

  
make_primelist (odd_numbs 1) [2];
fun smoothie n = so_smooth (make_primelist (odd_numbs n) [2]) n [];

fun smooth b i j =
  if i=j then 1
  else
    if (smoothie i) <= b then (1 + smooth b (i+1) j) else smooth b (i+1) j;
smooth 2 1 42;






(*

fun append_primes n nil = append_primes n [2]
|   append_primes 4 m = m
|   append_primes n m = if isprime n m then append_primes n-1 (n::m) else append_primes (n-1) m;

append_primes 10 [2];



*)
