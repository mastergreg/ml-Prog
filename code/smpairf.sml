fun smalpairfirst (x as (a,b) :: xs) = if a<b then (a,b)::(smalpairfirst xs)  else (b,a)::(smalpairfirst xs)
|   smalpairfirst nil = nil;
val a = [(1,2),(4,4),(4,5),(5,4),(7,4),(4,3)];
smalpairfirst a;
