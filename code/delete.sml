fun delete x nil = nil 
|   delete x (s::ss) = if x = s then delete x ss else s:: delete x ss;
val one = [1,2,3,4,5,3,5];
delete 3 one;
delete 10 one;
