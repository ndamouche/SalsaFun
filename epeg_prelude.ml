
open SalsaTypes ;;
open Epeg_types ;;
open Prelude ;;


let rec eqPegNode pn1 pn2 = match (pn1,pn2) with
  (PEGLeaf(e1),PEGLeaf(e2)) -> eqExpr e1 e2
| (PEGNode(op1,pnl1),PEGNode(op2,pnl2)) -> (op1 = op2) && (eqPegNodeRefList pnl1 pnl2)
| (BoxNode(op1,ab1,eqc1),BoxNode(op2,ab2,eqc2)) -> (op1 = op2) && (eqAbstractBox !ab1 !ab2) && (eqEqClass !eqc1 !eqc2)
| (GlobalBox(ab1),GlobalBox(ab2)) -> eqAbstractBox !ab1 !ab2 
| _ -> false 

and eqEqClass eqc1 eqc2 = match (eqc1,eqc2) with
  (None(c1),None(c2)) -> true
| (Singloton(pn1),Singloton(pn2)) -> eqPegNode !pn1 !pn2
| (EqClass(pnl1,eqcl1,abl1),EqClass(pnl2,eqcl2,abl2)) -> (eqPegNodeRefList !pnl1 !pnl2) && 
						         (eqEqClassRefList !eqcl1 !eqcl2) && 
							 (eqAbstractBoxRefList !abl1 !abl2) 
| _ -> false 

and eqAbstractBox ab1 ab2 = match (ab1,ab2) with
  (Box(op1,bcl1),Box(op2,bcl2)) -> (op1 = op2) && (eqBoxContentRefList bcl1 bcl2)

and eqBoxContent bc1 bc2 = match (bc1,bc2) with
  (VLeaf(e1),VLeaf(e2)) -> eqExpr e1 e2
| (VNode(pn1),VNode(pn2)) -> eqPegNode pn1 pn2
| (Box_(ab1),Box_(ab2)) -> eqAbstractBox !ab1 !ab2
| _ -> false 

and eqPegNodeRefList pnl1 pnl2 = match (pnl1,pnl2) with
  ([],[]) -> true
| (x::xs,y::ys) -> (eqPegNode !x !y) && (eqPegNodeRefList xs ys)
| _ -> false 

and eqEqClassRefList eqcl1 eqcl2 = match (eqcl1,eqcl2) with
  ([],[]) -> true
| (x::xs,y::ys) -> (eqEqClass !x !y) && (eqEqClassRefList xs ys)
| _ -> false 

and eqAbstractBoxRefList abl1 abl2 = match (abl1,abl2) with
  ([],[]) -> true
| (x::xs,y::ys) -> (eqAbstractBox !x !y) && (eqAbstractBoxRefList xs ys)
| _ -> false 

and eqBoxContentRefList bcl1 bcl2 = match (bcl1,bcl2) with
  ([],[]) -> true
| (x::xs,y::ys) -> (eqBoxContent !x !y) && (eqBoxContentRefList xs ys)
| _ -> false 
;;





