datatype asdfbe = None | Some of int;
fun head (h::t) = (Some h)
| head [] = None




datatype 'a tree = Null | Node of 'a * 'a tree * 'a tree;
val t = Node (12,Null,(Node (76,Null,Null)));

(*|   sumtree (Node (x,Null,Null)) = x*)

fun sumtree Null = 0
|   sumtree (Node (x,l,r)) = x+sumtree l + sumtree r;





fun insert_sorted_bad n nil = [n]
|   insert_sorted_bad n (h::tail) = if n = h then h::tail else
                                if n <  h then n::h::tail
                                else h::insert_sorted_bad n tail;
fun insert_sorted n lista =
  let
    fun insert_sorted_h n       nil acc = (rev acc) @ [n]
    |   insert_sorted_h n (h::tail) acc =
        if n < h then (rev acc) @ (n::h::tail)
        else if n = h then (rev acc) (h::tail)
        else insert_sorted_h n tail (h::acc)
  in 
    insert_sorted_h n lista nil
  end






fun d n = n + sumofdigits n
and sumofdigits 0 = 0 
|    sumofdigits n = (n mod 10) + sumofdigits(n div 10);




fun d n =
  let
     fun d_h n acc = if n < 10 then n + acc else
                      d_h (n div 10) (acc+(n mod 10))
  in
    d_h n n
  end

fun hit n =
  let
    fun hit_h i n lista = 
        if i > n then lista
        else hit_h (i+1) n (insert_sorted (d i) lista)
  in
    hit_h 1 n nil
  end
