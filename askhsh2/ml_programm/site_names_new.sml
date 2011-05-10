datatype trie = Head of (trie list)| Empty | Node of ((trie list)*int*char*int*int);
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
fun existsInTrie (ls:trie) (cht:char) =
    let
      fun existsInTrieH (Empty) cht:char = false
      |   existsInTrieH (children,ch,counter,level,chs) (cht:char) = ch=cht
    in
      existsInTrieH ls cht
    end
(*
fun existsInChildren (Head (ls)) (cht:char) = 
|   existsInChildren (Node (children,ch,counter,level,chs)) (cht:char) = 
*)
fun increaseTrieCnT (ls,ch,counter,level,chs)  = (ls,ch,counter+1,level,chs)
    
fun appendToTrie (ls,ch,count,level,chs) (cht:char) =((([],cht,1,level+1,0)::ls),ch,count,level,chs+1)
    
    
fun retrightChild (father)(ls) (cht:char) =
  let
    fun retrifhtChildH (father) [] (cht:char) = appendToTrie father cht
    fun retrifhtChildH (father) (l::ls) (cht:char) =
      if existsInTrie l cht then increaseTrieCnT l else retrifhtChildH father ls cht
  in
    retrifhtChildH father ls cht
  end
(*
fun nextChild level ([]) ch = [([],ch,1,level+1,0)]
|   nextChild level (l::ls) ch = 
  if existsInTrie l ch then (increaseTrieCnT l)::ls else l::nextChild level ls ch
*)

fun myparseStart file = 
  let
    val input = TextIO.openIn file
    val garbage = Option.valOf (TextIO.inputLine input)
    fun myparse () = Option.valOf (TextIO.input1 input)
    fun filldom (x:char) (trs:trie) = existsInTrie trs x
  in
    filldom (myparse () ) (Empty)
  end


fun site_names file =
 let
  val ls = myparseStart file
 in
  ls
 end

fun put_in [] cht = [(cht,1)]
|   put_in ((ch,cnt)::ls) cht = if ch=cht then (ch,cnt+1)::ls else (ch,cnt)::put_in ls cht




fun get_head_of_strings level ls ks = 
  let
    fun ghosH level [] acc (remainingstring:string list) = (level,acc,remainingstring)
    |   ghosH level (""::ls)  acc remainingstring = ghosH level ls acc remainingstring
    |   ghosH level (l::ls)  acc remainingstring = ghosH level ls (put_in acc (hd(explode(l)))) ((implode(tl(explode(l))))::remainingstring)
  in
    ghosH level ls ks []
  end




fun fillalllevels (sls:string list) =
  let
    fun nextLevel (level,acc,[]:string list) = (level,(acc,[]))
    |   nextLevel (level,acc,sls) = nextLevel (get_head_of_strings (level+1) sls acc)
  in
    nextLevel (1,[],sls)
  end

