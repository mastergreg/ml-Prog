fun popi (0, x::xs) = xs
|   popi (i, nil) = nil
|   popi (i, x::xs) = x::popi(i-1,xs);
val a = [#"a", #"s", #"d", #"f", #"a", #"s", #"d", #"f"];
popi(20,a);
