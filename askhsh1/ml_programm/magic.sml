fun next_test (b:int) (l:int list) = 
  let
    fun next_test_h b l _ 0       = l
    |   next_test_h b l 0 _       = l
    |   next_test_h b (h::t) (c:int) (i:int)  = ((h+c) mod b ) :: next_test_h b t ((h+c) div b) i-1
  in
    next_test_h b l 1 length(l)
  end
(*
fun ismagic b l =
*)
