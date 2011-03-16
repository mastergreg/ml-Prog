fun cube x:real = x*x*x;
fun smallest3 (x:real,y,z) =
  if x<y then
    if x<z then x
    else z
  else 
    if y<z then y
    else z;
val testing = (10.0,2.0,3.0);
fun third m = hd(tl(tl(m)));
val test = [1,2,3,4,5];
fun thirdchar mystr = third (explode mystr);
val astr="hithere";
fun cyclelist m = tl(m) @ [hd(m)];
cyclelist test;
