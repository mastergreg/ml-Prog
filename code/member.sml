
fun member _ nil = false
|   member x (s::ss)  = (x = s) orelse member x ss;
val one = [1,2,3,4,5,3,5];
val re = [1.2,1.3,1.5,1.6];
val ch = explode("asdfasdf");




member (3,one);
member (10,one);
member (#"a",ch);
member (#"e",ch);
member (1.5,re);
member (2.4,re);
