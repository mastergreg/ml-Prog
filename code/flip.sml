fun flipper nil = nil
|   flipper (a::nil) = [a]
|   flipper (a::(b::nil)) = [b,a]
|   flipper (a::(b::bs)) = [b,a]  @ flipper bs;
val a = explode "asdfasdf";
flipper a;
