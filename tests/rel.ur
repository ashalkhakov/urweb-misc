con heading = [A = int, B = int]
con detail = [A = int, C = int, F = string]

fun main () =
    let
	val (o, emp) = Relation.empty [heading]
	val xs = Relation.insert o {A = 5, B = 5} emp (* (4,5)(4,6)(5,5)(6,5)
(4,5)<(4,6) because 4=4 && 5<6
(4,6)<(5,5) because 4<5 && 6>=5
(5,5)<(6,5) because 5<6 && 5=5

A<B iff fst A < fst B
A<B iff fst A = fst B && snd A < snd B
A<B iff snd A < snd B

*)
	val xs = Relation.insert o {A = 4, B = 5} xs (* A < B iff A.l < B.l && rest(A) >= rest(B) *)
	val xs = Relation.insert o {A = 4, B = 6} xs
	val xs = Relation.insert o {A = 6, B = 5} xs
	val xs = Relation.insert o {A = 6, B = 6} xs
	val (oy, ys) = Relation.project [[A=int]] xs
	val (oz, zs) = Relation.empty [detail]
	val zs = Relation.insert oz {A=4, C=10, F="first"} zs
	val zs = Relation.insert oz {A=5, C=10, F="second"} zs
	val zs = Relation.insert oz {A=7, C=10, F="third"} zs
	val (or, rs) = Relation.join [[A=int]] xs zs
	val (oxs1, xs1) = Relation.project [[B=int]] xs
	val (or1, rs1) = Relation.times xs1 zs
    in (* (a,b) < (c,d) <-> a<c && b<d *)
	(*(1,2) < (2,2) because 1<2 *)
	(*(2,2) >= (2,2) because not 2<2 *)
	(*(4,5) < (4,6) because 5<6 *)
(*
 and also {[lt {A=4, B=5} {A=4, B=6}]} and {[gt {A=4,B=5} {A=4,B=6}]}
 and also {[lt {A=4,B=4} {A=5,B=4}]} and {[gt {A=4,B=4} {A=5,B=4}]}*)
	return <xml>
	  <body>
	    <p>Relation xs =</p>
	    {Relation.tablize_rel
		  {A="A", B="B"}
		  xs}

	    <p>project<sub>A</sub> xs</p>
	    {Relation.tablize_rel
		  {A="A"}
		  ys}

	    <p>while we also have:</p>
	    {[oy {A=4} {A=4}]}<br/>
	    {[oy {A=4} {A=5}]}<br/>
	    {[oy {A=5} {A=6}]}<br/>
	    <br/>

	    <p>Relation zs =</p>
	    {Relation.tablize_rel
		  {A="A", C="C", F="F"}
		  zs}
	    
	    <p>join xs zs</p>
	    {Relation.tablize_rel
		  {A="A", B="B", C="C", F="F"}
		  rs}

	    <p>project<sub>B</sub>xs</p>
	    {Relation.tablize_rel
		  {B="B"}
		  xs1}
	    
	    <p>xs * zs</p>
	    {Relation.tablize_rel
		  {A="A",B="B",C="C",F="F"}
		  rs1}

            </body>
	</xml>
    end
