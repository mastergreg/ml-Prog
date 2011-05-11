(*datatype trie = Empty | Head of (trie list) | Node of (trie list*char*int*int*int);
datatype tr = Tr of (string*char*int);*)
fun parse file =
  let
    (* Open input file *)
    val input = TextIO.openIn file
    (* Hocus pocus read an integer *)
    val n = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
    (* Clean the new line after the integer *)
    val garbage = TextIO.inputLine input
    fun read_name 0 acc = rev acc
      | read_name i acc =
        let
	  (* Read a string *)
          val rawName = Option.valOf (TextIO.inputLine input)
	  (* Remove the \n in the end of the line *)
          val name = String.implode (rev (tl (rev (String.explode rawName))))
        in
          read_name (i-1) (name::acc)
        end
  in
    read_name n nil
  end
(*fun fill ls = *)
(*
fun existsInTrie (ls:trie) (cht:char) =
    let
      fun existsInTrieH Empty _ = false
      |   existsInTrieH (Head(ls)) _ = false
      |   existsInTrieH (Node (children,ch,counter,level,chs)) (cht:char) = ch=cht
    in
      existsInTrieH ls cht
    end

fun increaseTrieCnT (Node (ls,ch,counter,level,chs))  = (Node (ls,ch,counter+1,level,chs))
|   increaseTrieCnT (l:trie)  = l 
    
fun appendToTrie (Node (ls,ch,count,level,chs)) (cht:char) =(Node ((Node ([],cht,1,level+1,0)::ls),ch,count,level,chs+1))
|   appendToTrie (l:trie) (cht:char) = l
    
    
*)
(*
fun nextTlist [] c level = [(Node ([],c,1,level,0))]
|   nextTlist (t::tls) c level = if existsInTrie t c then ((increaseTrieCnT t)::tls) else (t::(nextTlist tls c level))
*)
(*
fun fillnames str:string tr:trie = 
  let
    val name = explode (str)
    fun fillnamesH []   tri:trie = tri
    |   fillnamesH (n::ame) Head(ls) = (Head (nextTlist ls n 0)
    |   fillnamesH (n::ame) tri:trie = 
      

*)
(*
fun myparseStart file = 
  let
    val input = TextIO.openIn file
    val garbage = Option.valOf (TextIO.inputLine input)
    fun myparse () = Option.valOf (TextIO.input1 input)
    fun filldom (x:char) (trs:trie) = existsInTrie trs x
  in
    filldom (myparse () ) (Head [])
  end
*)

fun isin (prefix,counter) [] = counter
|   isin ("",counter) _ = 0
|   isin (prefix,counter) ((st::sts):string list) = 
  if prefix=st then isin (prefix,counter+1) sts else isin (prefix,counter) sts


fun maxInstances pls = 
  let
    fun maxInstancesH [] max = max
    |   maxInstancesH (p::pls) max = 
      let
        val buf = isin (p,1) pls
      in
        if max<buf then maxInstancesH pls buf else maxInstancesH pls max
      end
  in
    maxInstancesH pls 0
  end



fun buildPrefix (st:string) = 
  let
    val lst = explode st
    fun buildPrefixH  [] pref acc = rev acc
    |   buildPrefixH (l::lst) pref acc =
      let
        val prefix=pref^str(l)
      in
        buildPrefixH lst prefix (prefix::acc)
      end
    in
      buildPrefixH lst "" []
    end

fun testList (st:string) ([]:string list) = 0
|   testList (st:string) ((s::ls):string list) = 
  if (st=s) then size(st) else 
    if String.isPrefix st s then  testList st ls else 0
     
fun ls (ass:string list) (bss:string list) =
  let
    fun lsH [] _ acc = acc
    |   lsH _ [] acc = acc
    |   lsH ((a::ass):string list) ((b::bss):string list) acc = 
      if a=b then lsH ass bss acc+1 else acc
  in
    lsH ass bss 0
  end
  


fun mhd [] = ""
|   mhd ls = hd(ls)
fun get_Heads sls = map mhd sls 


fun mtd [] = []
|   mtd ls = tl(ls)
fun get_Tails sls = map mtd sls 



fun site_names file =
 let
  val ls = parse file

 in
   ls
 end
(*
fun nerves = t:tr list chls:string list = 
  val nervesH [] chls = map
*)

fun put_in [] cht = [(cht,1)]
|   put_in ((ch,cnt)::ls) cht = if ch=cht then (ch,cnt+1)::ls else (ch,cnt)::put_in ls cht

(*
fun get_head_of_strings (ls:string list) (ks:char*int) = 
  let
    fun ghosH [] acc remainingstring = (acc:(char*int) list,remainingstring:string list)
    |   ghosH (l::ls)  acc remainingstring = ghosH ls (put_in acc (hd(explode(l)))) ((implode(tl(explode(l))))::remainingstring)
  in
    ghosH ls ks []
  end




fun fillalllevels sls =
  let
    fun nextLevel level (acc,[]:string list) = (level,(acc,[]:string list))
    |   nextLevel level (acc,sls) = nextLevel (level+1) (get_head_of_strings(sls,acc))
  in
    nextLevel 1 ([],sls)
  end
*)

fun makeNList n = 
  let
    fun makeNListH 0 acc  = acc
    |   makeNListH n acc = makeNListH (n-1) ([]::acc)
  in
    makeNListH n []
  end



fun main_test file = 
  let
    val sls = parse file
    val prefixes = (map buildPrefix sls)
    fun main_testH ([]:string list list) (max:int) (level:int)= max 
    |   main_testH prefixes max level= 
      if prefixes = makeNList (length prefixes)  then max else
      let
        val buf = (maxInstances (get_Heads prefixes))*level
      in
        if max<buf then main_testH (get_Tails prefixes) buf (level+1) else main_testH (get_Tails prefixes) max (level+1)
      end
  in
    main_testH prefixes 0 1
  end
       
      
