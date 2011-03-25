fun member _ nil = false
|   member x (s::ss)  = (x = s) orelse member x ss;
fun insert x nil = [x]
|   insert x m = if member x m then m else x::m;
fun isert a nil = nil
|   isert a (l::ls) = (insert a l)::isert a ls;



fun power_set nil = nil
|   power_set (l::ls) =[l]::power_set(ls);

val mls = [[1,2,3],[2,3,4],[5,5,3],nil];
isert 1 mls;
val els = [5,2,3];
power_set els;
