(*FIXME to show fields, need strings for each field*)

datatype cmp = Lt | Eq | Gt (* FIXME why is ord so strange in the base library? *)
val show_cmp = mkShow (fn x => case x of Lt => "Lt" | Eq => "Eq" | Gt => "Gt")
fun compare [t ::: Type] (o : ord t) : t -> t -> cmp =
 fn x y => if x < y then Lt else if x <= y then Eq else Gt

(* ****** ****** *)

con subst' (n :: Name) (m :: Name) (t :: Type) (r :: {Type}) =
		      [[n] ~ r] => [[m] ~ r] =>
    $([n=t] ++ r)
    -> $([m=t] ++ r)

fun subst' [n :: Name] [m :: Name] [t :: Type] [r ::: {Type}]
	   [[n] ~ r] [[m] ~ r]
	   (x : $([n=t] ++ r)) : $([m=t] ++ r) = let
				     val t = x.n
				 in
				     x -- n ++ {m=t}
				 end

con subst = fn (s :: {Name}) (r1 :: {Type}) (r2 :: {Type}) =>
	       (* show that s is N steps which turn r1 into r2 -- that is, we proceed by induction on s *)
	       tf :: ({Name} -> {Type} -> {Type} -> Type)
	       (* inductive step: it simply replaces one name based on its substitution *)
	       -> (n :: Name -> m :: Name -> t :: Type -> sub :: {Name} -> rest1 :: {Type} -> rest2 :: {Type}
		   -> [[n]~[m]] => [[n]~rest2] => [[m]~rest2] => [[n]~sub] =>
		   (*t -> FIXME?*) tf sub rest1 rest2 -> tf (sub++[n=m]) ([n=t]++rest2) ([m=t]++rest2))
	       (* base step: no change *)
	       -> (r :: {Type} -> tf [] r r)
	       -> tf s r1 r2

structure Subst = struct
    fun nil [r ::: {Type}] [tf :: ({Name} -> {Type} -> {Type} -> Type)]
	(f : n :: Name -> m :: Name -> t :: Type -> sub :: {Name} -> rest1 :: {Type} -> rest2 :: {Type}
	     -> [[n]~[m]] => [[n]~rest2] => [[m]~rest2] => [[n]~sub] =>
	 tf sub rest1 rest2 -> tf (sub++[n=m]) ([n=t]++rest2) ([m=t]++rest2))
	(i : r :: {Type} -> tf [] r r) = i [r]
    fun cons [r ::: {Type}] [n :: Name] [m :: Name] [t :: Type]
	     [[n] ~ r] [[m] ~ r] [[n]~[m]]
	     [tf :: ({Name} -> {Type} -> {Type} -> Type)]
	(f : n :: Name -> m :: Name -> t :: Type -> sub :: {Name} -> rest1 :: {Type} -> rest2 :: {Type}
	     -> [[n]~[m]] => [[n]~rest2] => [[m]~rest2] => [[n]~sub] =>
	 tf sub rest1 rest2 -> tf (sub++[n=m]) ([n=t]++rest2) ([m=t]++rest2))
	(i : r :: {Type} -> tf [] r r) = f [n] [m] [t] [[]] [r] [r] (i [r])
(*
FIXME runtime behaviour? basically we want $r1 -> $r2 but how?
TODO composition of substitutions
*)
end

(* ****** ****** *)

con relvar = fn r :: {Type} => list $r
con order = fn (t :: Type) => t -> t -> cmp

fun make_order [r ::: {Type}] (fl : folder r) (o : $(map ord r)) : order $r =
    @foldR3 [ord] [ident] [ident] [fn _ => cmp]
     (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] isLt x y acc =>
	 case @compare isLt x y of Lt => Lt | Eq => acc | Gt => Gt)
     Eq fl o

fun empty [r :: {Type}] (fl : folder r) (o : $(map ord r)) = (@make_order fl o, [])

fun insert_list [t ::: Type] (cmp : t -> t -> cmp) (x0 : t) (xs : list t) : list t = let
    fun aux (xs : list t) (flag : int) : (int * list t) =
	case xs of
	    x :: xs1 =>
	    (case cmp x0 x of
		 Gt => (flag+1, x0 :: xs)
	       | Lt => let
		     val flag0 = flag
		     val (flag, xs1) = aux xs1 flag
		 in
		     (flag, if flag = flag0 then xs else x :: xs1)
		 end
	       | Eq => (flag+1, xs))
	  | [] => (flag+1, x0 :: [])
	val (_, ext) = aux xs 0
in
    ext
end

fun insert [r ::: {Type}] (o : order $r) (re : $r) (rv : relvar r) : relvar r =
    insert_list o re rv

fun mem [r ::: {Type}] (o : order $r) (x0 : $r) (rv : relvar r) : bool = let
  fun aux (xs : list $r) : bool = case xs of
    x :: xs => (case o x0 x of Gt => False | Lt => aux xs | _ => True)
  | [] => False
in
  aux rv
end

fun project [r1 :: {Type}] [r2 ::: {Type}] [r1 ~ r2]
	    (fl : folder r1) (o : $(map ord r1))
	    (rv : relvar (r1++r2)) : order $r1 * relvar r1 = let
  val i = Incl.incl [r1] [r2]
  val order = @@make_order [r1] fl o
  fun aux (acc : relvar r1) (inp : list $(r1++r2)) : relvar r1 =
      case inp of
	  [] => acc
	| x :: inp => aux (insert order (Incl.proj i x) acc) inp
in
    (order, aux [] rv)
end

fun restrict [r ::: {Type}] (pred : $r -> bool) (rv : relvar r) : relvar r = List.filter pred rv

fun union [r ::: {Type}] (o : order $r) (l : relvar r) (r : relvar r) : relvar r = let
    fun aux0 (acc : list $r) (xs : list $r) =
	case xs of [] => acc | x :: xs => aux0 (insert o x acc) xs
    fun aux (acc : list $r) (l : list $r) (r : list $r) : list $r =
	case (l, r) of
	    ([], _) => aux0 acc r
	  | (_, []) => aux0 acc l
	  | (x :: l, y ::r) => aux (insert o y (insert o x acc)) l r
in
   aux [] l r
end

fun difference [r ::: {Type}] (o : order $r) (l : relvar r) (r : relvar r) : relvar r = let
    fun aux (acc : list $r) (xs : list $r) : list $r =
	case xs of
	    [] => acc
	  | x :: xs => if mem o x r then aux acc xs else aux (insert o x acc) xs
in
    aux [] l
end

fun join [r1 ::: {Type}] [r2 ::: {Type}] [r3 :: {Type}] [r1 ~ r2] [r2 ~ r3] [r1 ~ r3]
	 (fl1 : folder (r1++r2)) (o1 : $(map ord (r1++r2)))
	 (fl : folder r3) (o : $(map ord r3)) (fl2 : folder (r1++r2++r3))
	 (l : relvar (r1++r3)) (r : relvar (r2 ++ r3)) : order $(r1++r2++r3) * relvar (r1 ++ r2 ++ r3) = let
    val i1 = Incl.incl [r3] [r1]
    val i2 = Incl.incl [r3] [r2]
    val i3 = Incl.incl [r2] [r3]
    val order = @@make_order [r1++r2++r3] fl2 (o1++o)
    val korder = @@make_order [r3] fl o
    fun loop (acc : relvar (r1++r2++r3)) (k : $r3) (x : $(r1++r3)) (r : relvar (r2++r3)) : relvar (r1++r2++r3) =
	case r of
	    [] => acc
	  | y :: r => let
		val k' = Incl.proj i2 y
	    in
		case korder k k' of
		    Lt => loop acc k x r
		  | Eq => loop (insert order (x ++ (Incl.proj i3 y)) acc) k x r
		  | Gt => loop acc k x r
	    end
	
    fun aux (acc : relvar (r1++r2++r3))
	    (l : relvar (r1++r3)) (r : relvar (r2++r3)) : relvar (r1++r2++r3) =
	    case l of
		[] => acc
	      | x :: l => let
		    val k = Incl.proj i1 x
		    val acc = loop acc k x r
		in
		    aux acc l r
		end
in
    (order, aux [] l r)
end

fun times [r ::: {Type}] [s ::: {Type}] [r ~ s]
	  (fl : folder (r++s)) (o1 : $(map ord r)) (o2 : $(map ord s))
	  (rv : relvar r) (rs : relvar s) : order $(r++s) * relvar (r++s) = let
    val order = @@make_order [r++s] fl (o1++o2)
    fun loop (acc : relvar (r++s)) (x : $r) (rs : relvar s) : relvar (r++s) =
	case rs of
	    [] => acc
	  | y :: rs => loop (insert order (x++y) acc) x rs
    fun aux (acc : relvar (r++s))
	(rv : relvar r) (rs : relvar s) : relvar (r++s) =
	case rv of
	    [] => acc
	  | x :: rv => aux (loop acc x rs) rv rs
in
    (order, aux [] rv rs)
end

fun show_rel [r ::: {Type}] (sh : $r -> string) (rv : relvar r) : string = let
    fun f ls = case ls of [] => "" | x :: ls => sh x ^ "; " ^ f ls
in
    f rv
end

fun tablize_rel [r ::: {Type}]
		(fl : folder r)
    (heading : $(map (fn _ => string) r))
    (sh : $(map show r))
    (rv : relvar r) =
	  <xml>
	    <table>
	      <tr>{@mapX [fn _ => string] [[Tr,Dyn,MakeForm]]
	       (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest] name =>
		   <xml><td>{[name]}</td></xml>)
	       fl
	       heading
}</tr>
	      {List.mapX
		   (fn x => <xml>
		     <tr>
		       {@mapX2
			 [fn x => x] [show] [[Tr,Dyn,MakeForm]]
			 (fn [nm :: Name] [t ::_]
					  [rest ::_]
					  [[nm] ~ rest]
					  v fsh => <xml>
					    <td>{[@show fsh v]}</td>
					  </xml>)
			 fl x sh}
		     </tr>
		   </xml>) rv}
	    </table>
	  </xml>

