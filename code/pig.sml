fun cyclelist (nil) = nil
|   cyclelist (m::ms) = ms @ [m]
fun vowfirst nil = false
|   vowfirst (a::_) = a = #"a" orelse a = #"e" orelse a = #"i" orelse a = #"o" orelse a = #"u" orelse a = #"y";
  fun pig_latin w= 
if vowfirst (explode w) 
  then  w ^ "yay" 
  else 
    let
      fun pig_latin2 a = 
        if vowfirst(explode a) 
        then a ^ "ay"
        else pig_latin2 ((implode(cyclelist(explode a))))
    in pig_latin2 w
    end

  val a = "able";
  val b = "stripe"; 
  (*
   vowfirst(explode a);
   vowfirst(explode b);
   cyclelist(explode a);
   *)
  pig_latin a;
  pig_latin b;

