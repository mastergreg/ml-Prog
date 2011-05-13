
fun quicksort l = 
  let
    fun split pivot [] lower upper s = qs lower (pivot :: (qs upper s))
    |   split pivot (x::xs) lower upper s = 
      if x < pivot
        then split pivot xs (x::lower) upper s
        else split pivot xs lower (x::upper) s
    and
        qs []  s          = s
    |   qs [x] s          = (x::s)
    |   qs (pivot::rest) s = split pivot rest [] [] s
  in
    qs l []
  end
fun nextTest (b:int) (l:int list) = 
  let
    fun nextTesth _ [] _ = [] 
    |   nextTesth _ l 0  = l
    |   nextTesth b (h::t) c    = 
            let
              val dig = if (h+c) < b then h+c else 0
              val c = if (h+c) < b then 0 else 1
            in
              dig::nextTesth b t c
            end
  in
    nextTesth b l 1
  end
fun extraTest (b:int) (l:int list) = 
  let
    fun extraTestH b [] p acc     = acc
    |   extraTestH b (h::t) p acc = if h >= p then extraTestH b t h (h::acc) else extraTestH b t p (p::acc)
  in
    extraTestH b (rev l) 0 []
  end
    

fun subtract b xs ys = 
  let
    fun subh _ [] _ _ acc = acc
    |   subh _ _ [] _ acc = acc
    |   subh b (x::xs) (y::ys) c acc = 
    let 
      val al = x-y-c
      val dig = if al<0 then al+b else al
      val c = if al < 0 then 1 else 0
    in
     subh b xs ys c (dig::acc)
    end
  in
    subh b xs ys 0 nil
  end 
  
fun ismagic b l =
  let
    val sl = quicksort l
    val ls = rev sl
  in
    subtract b sl ls = l
  end
fun checkZero n l = 
  let
    val ndiv2 = (n div 2)
    fun checkZeroH n h = List.nth (h,n)>0
  in
    checkZeroH ndiv2 l
  end
fun makeNList n = 
  let
    fun makeNListH 0 acc  = acc
    |   makeNListH n acc = makeNListH (n-1) (0::acc)
  in
    makeNListH n []
  end
fun fancyprint n b l = 
  let

    fun fancyprintH 0 (b:int) ((l:int)::ls) (acc:IntInf.int)= acc 
    |   fancyprintH _ (b:int) []      (acc:IntInf.int)= acc 
    |   fancyprintH n (b:int) ((l:int)::ls) acc= 
      fancyprintH (n-1) b ls ((IntInf.pow((Int.toLarge(b)),n-1)*(Int.toLarge(l)))+acc)
  in
    fancyprintH n b l 0
  end

fun magic b n =
  let
    val start = (nextTest b (makeNList n))
    fun magicH n b l = 
      if checkZero n l then [0] 
        else 
            let
              val candidate = subtract b (rev l) l
            in
              if ismagic b candidate then candidate else magicH n b (extraTest b (nextTest b l))
            end
  in
    let
      val top = magicH n b start
    in
      fancyprint n b top 
    end
  end
fun main() =
  let
    val t= (CommandLine.arguments())
    val b = hd(t)
    val n = hd(tl(t))
    val iti = magic (Option.valOf (Int.fromString b)) (Option.valOf (Int.fromString n))
    val st = LargeInt.toString iti
  in 
    print (st^"\n")
  end
val _ = main ()


