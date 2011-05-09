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
fun existsInTrie (ls) (cht:char) =
    let
      fun existsInTrieH (children,ch,counter,level,chs) (cht:char) = ch=cht
    in
      existsInTrieH ls cht
    end

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

fun nextChild level ([]) ch = [([],ch,1,level+1,0)]
|   nextChild level (l::ls) ch = 
  if existsInTrie l ch then (increaseTrieCnT l)::ls else l::nextChild level ls ch




fun site_names file =
 let
  val ls = parse file
 in
   ls
 end
