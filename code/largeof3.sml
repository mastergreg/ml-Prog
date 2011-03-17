fun largeof3 nil = 0.0
|   largeof3 (x::nil) = x
|   largeof3 (x::xs) = if x >= largeof3 xs then x else largeof3(xs);
val m=[1.2,1.3,1.6,1.7,2.3];
largeof3(m);
