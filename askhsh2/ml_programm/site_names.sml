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
fun quicksort [] = []
|   quicksort (p::lst) = 
      let fun quicksort_r pivot ([], front, back) =  (quicksort front) @ [pivot] @ (quicksort back)
          |   quicksort_r pivot (x::xs, front, back) = 
                if x < pivot then 
                   quicksort_r pivot (xs, x::front, back)
                else 
                   quicksort_r pivot (xs, front, x::back)
      in
         quicksort_r p (lst, [], [])
      end
(*
fun isin prefix counter [] = counter
|   isin "" counter _ = 0
|   isin prefix counter ((st::sts)) = 
  if prefix=st then isin prefix (counter+1) sts else isin prefix counter sts

*)

fun mhdc [] = ""
|   mhdc ls = str(hd(ls))
fun get_HeadsC cls = map mhdc cls 

fun mhdS [] = ""
|   mhdS ls = hd(ls)

fun mtlc [] = [] 
|   mtlc ls = tl(ls)
fun get_TailsC cls = map mtlc cls 

fun conStrList l1 l2 = 
  let
    fun conStrListH [] _ acc = rev acc
    |   conStrListH _ [] acc = rev acc
    |   conStrListH (l1::ls1) (l2::ls2) acc = conStrListH ls1 ls2 ((l1^l2)::acc)
  in
    conStrListH l1 l2 nil
  end


fun isin (prefix,counter) counter_old [] = if counter>counter_old then counter else counter_old
|   isin ("",_) _  _ = 0
|   isin (prefix,counter) counter_old ((st::sts)) = 
  if prefix=st 
    then 
      isin (prefix,(counter+1)) counter_old sts 
    else
      let
        val max = if counter_old<counter then counter else counter_old
      in
        isin (mhdS(sts),1) max (mtlc(sts))
      end

fun maxInstances pls = 
  let
    val test = quicksort pls
    fun maxInstancesH [] max = max
    |   maxInstancesH (p::pls) max = 
      isin (p,1) 0 pls
  in
    maxInstancesH test 0
  end

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
    val boom = map explode sls
    val suffixes = get_TailsC boom
    val prefixes = get_HeadsC boom 
    val endyou = makeNList (length suffixes)
    fun main_testH prefixes suffixes max level= 
      let
        val buf = ((maxInstances  prefixes)*(level))
        val newmax = if max<buf then buf else max
      in
        if suffixes = endyou  
          then 
            newmax
          else
            main_testH (conStrList prefixes (get_HeadsC suffixes)) (get_TailsC suffixes) newmax (level+1) 
      end
  in
    (main_testH prefixes suffixes 0 1)
  end
(*
fun main() =
  let
    val t= (CommandLine.arguments())
    val b = hd(t)
    val iti = main_test b
    val st = Int.toString iti
  in 
    print (st^"\n")
  end
val _ = main ()
*)


fun site_names file = main_test file
