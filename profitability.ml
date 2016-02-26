
open Num ;;
open SalsaTypes ;;
open Epeg_types ;;
open Prelude ;;
open Epeg_prelude ;;
open Print ;;
open Float ;;

(******************************************************)

let aStreamSseq = feSseq ;;


let aStreamEPEGErrorSseq (f,e) (f',e') = match (e,e') with
  (J(a,b),J(c,d)) -> ((max (abs_float a) (abs_float b)) <= (max (abs_float c) (abs_float d)))  
| (J(_),JEmpty) -> false
| (JEmpty,J(_)) -> true
| (JEmpty,JEmpty) -> true
| (JInfty,JInfty) -> true
| (_,JInfty) -> true
| (JInfty,_) -> false
;;


let aStreamJoin   = feJoin ;;

let aStreamPlus   = fePlus ;;
let aStreamMinus  = feMinus ;;
let aStreamMult   = feMult ;;
let aStreamDiv    = feDiv ;;
let aStreamUMinus = feUMinus ;;
let aStreamSqrt = feSqrt ;;
let aStreamCos = feCos ;;
let aStreamSin = feSin ;;

let asTop    = (I(min_float,max_float),JInfty) ;;
let asBottom = (Empty,JEmpty) ;;


let emptyLustreLab = Lab(0) ;;

(******************************************************)


let profiteResEnv = ref [] ;;


let rec getProfitEnv id env = match env with
  [] -> raise (EPEGError("getProfitEnv","EPEGError"))
| (x,y)::xs -> if (eqEqClass !x !id) then y else getProfitEnv id xs ;;


let rec isDefEQC x xList = match xList with
  [] -> false
| (id,_)::ys -> if (eqEqClass !x !id) then true else isDefEQC x ys ;;


let list2Pair l = match l with
  a::b::[] -> (a,b)
| _ -> raise (EPEGError ("list2Pair","argument should be a list of exactly 2 elements")) ;;


let list2Triple l = match l with
  a::b::c::[] -> (a,b,c)
| _ -> raise (EPEGError ("list2Triple","argument should be a list of exactly 3 elements")) ;;


let rec pnMember pn pnList = match pnList with
  [] -> false
| x::xs -> if (eqPegNode !x !pn) then true else pnMember pn xs ;;
			
  
let rec getEnvProfit id env = match env with
		[] -> asBottom
	| 	(id',(_,v))::ideList -> if (eqEqClass !id !id') then v else getEnvProfit id ideList ;;
		
		
let rec sseqProfitEnv env1 env2 = match env1 with
	[] -> true
|   (id,(leRes,asRes))::env1' -> 	if (aStreamSseq asRes (getEnvProfit id env2)) then
										sseqProfitEnv env1' env2
							 		else 
								 		false ;;

let rec profiteEnvJoin env1 env2 = match env1 with
  [] -> env2
| (id,(leRes,asRes))::env1' -> 
	let asRes2 = getEnvProfit id env2 
	in profiteEnvJoin env1' (setEnv id (leRes,(aStreamJoin asRes asRes2)) env2) ;;
								

let rec getEqClass pn eqcList = match eqcList with
  [] -> Singloton(pn) 
  (* raise (EPEGError("getEqClass","pegNode "^(printPEGNode pn)^" does not occur in eqClasses below")) *)
| x::eqcList' -> (match !x with
					None(_) -> getEqClass pn eqcList'
				  | Singloton(pn') -> 
			  						if (eqPegNode !pn !pn') then 
										Singloton(pn')
									else 
										getEqClass pn eqcList'
				  | EqClass(pnList,ec,ab) ->
				  							if (pnMember pn !pnList) then 
												EqClass(pnList,ec,ab)							
											 else 
											    getEqClass pn eqcList'
				) ;;	


let rec getEnv2 id env = match env with
  [] -> raise (EPEGError ("getEnv2","cannot find eqclass in eqclassbelow:\n"))
| (id',v)::env' -> if (id=id') then v else (getEnv2 id env') ;;




let rec flattenEqClassEnv eqcEnv res = match eqcEnv with
[] -> res
| (_,eqc)::eqcEnv' -> flattenEqClassEnv eqcEnv' (eqc::res) ;;


let rec selectPN profiteEQC leRes asRes = match profiteEQC with
	[] -> (leRes,asRes)
| 	(_,(le,aS))::profiteEQC' -> if (aStreamEPEGErrorSseq aS asRes) then
                            		selectPN profiteEQC' le aS
				    else 
					selectPN profiteEQC' leRes asRes ;;

(*******************************************)


let rec removeBC x bclp = match bclp with 
  [] -> raise (EPEGError("removeBC","removebc must remove exactly 1 elt:"))
| bc::bclp' -> if (eqExpr (fst x) (fst bc)) then bclp'
                         else bc::(removeBC x bclp') ;;


let rec reifyPlusAux2 le aStr bclp leRes asRes = match bclp with
  [] -> (leRes,asRes)
| (le',aStr')::bclp' -> 
        let asNew = aStreamPlus aStr aStr' in
        let asOld = aStreamPlus aStr asRes 
        in  if (aStreamEPEGErrorSseq asNew asOld) then
                reifyPlusAux2 le aStr bclp' le' aStr'  
            else
                reifyPlusAux2 le aStr bclp' leRes asRes ;;


let rec reifyPlusAux1 bclpHead bclpTail leRes1 asRes1 leRes2 asRes2 = match bclpHead with
  [] -> (leRes1,asRes1,leRes2,asRes2)
| (le,aStr)::bclpHead' -> 
        let (newLe,newAs) = reifyPlusAux2 le aStr (bclpHead'@bclpTail) (Id("UNDEFINED++",emptyLustreLab)) asTop
        in  if (aStreamEPEGErrorSseq (aStreamPlus aStr newAs) (aStreamPlus asRes1 asRes2)) then
                reifyPlusAux1 bclpHead' ((le,aStr)::bclpTail) le aStr newLe newAs 
            else
                reifyPlusAux1 bclpHead' ((le,aStr)::bclpTail) leRes1 asRes1 leRes2 asRes2 ;;


let rec reifyPlus bcListProfite = match bcListProfite with
  [] -> ((Id("UNDEFINED++",emptyLustreLab)),asTop)
| (leRes,asRes)::[] -> (leRes,asRes)
| _ ->  let (le1,as1,le2,as2) = reifyPlusAux1 bcListProfite [] (Id("UNDEFINED++",emptyLustreLab)) asTop  (Id("UNDEFINED++",emptyLustreLab)) asTop in
        let newLe = Plus(le2,le1,emptyLustreLab) in
        let newAs = aStreamPlus as1 as2 in
        let newBclp = removeBC (le1,as1) (removeBC (le2,as2) bcListProfite)
        in reifyPlus ((newLe,newAs)::newBclp) ;;


let rec reifyMultAux2 le aStr bclp leRes asRes = match bclp with
  [] -> (leRes,asRes)
| (le',aStr')::bclp' -> 
        let asNew = aStreamMult aStr aStr' in
        let asOld = aStreamMult aStr asRes 
        in  if (aStreamEPEGErrorSseq asNew asOld) then
                reifyMultAux2 le aStr bclp' le' aStr'  
            else
                reifyMultAux2 le aStr bclp' leRes asRes ;;


let rec reifyMultAux1 bclpHead bclpTail leRes1 asRes1 leRes2 asRes2 = match bclpHead with
  [] -> (leRes1,asRes1,leRes2,asRes2)
| (le,aStr)::bclpHead' -> 
        let (newLe,newAs) = reifyMultAux2 le aStr (bclpHead'@bclpTail) (Id("UNDEFINED*3*",emptyLustreLab)) asTop
        in  
            let as1 = (aStreamMult aStr newAs) in
            let as2 = (aStreamMult asRes1 asRes2) in
            if (aStreamEPEGErrorSseq as1 as2) then
                reifyMultAux1 bclpHead' ((le,aStr)::bclpTail) le aStr newLe newAs 
            else
                reifyMultAux1 bclpHead' ((le,aStr)::bclpTail) leRes1 asRes1 leRes2 asRes2 ;;


let rec reifyMult bcListProfite = match bcListProfite with
  [] -> raise (EPEGError ("reifyMult","empty list: impossible case"))
| (leRes,asRes)::[] -> (leRes,asRes)
| _ ->  let (le1,as1,le2,as2) = reifyMultAux1 bcListProfite [] (Id("UNDEFINED*1*",emptyLustreLab)) asTop  (Id("UNDEFINED*2*",emptyLustreLab)) asTop in
        let newLe = Times(le2,le1,emptyLustreLab) in
        let newAs = aStreamMult as1 as2 in
        let newBclp = removeBC (le1,as1) (removeBC (le2,as2) bcListProfite)
        in reifyMult ((newLe,newAs)::newBclp) ;;


let rec combinePlusAux aStr1 le1 cl2 leRes aStrRes = match cl2 with
  [] -> (leRes,aStrRes)
| (lExpr2,aStr2)::cl2' -> let r = aStreamPlus aStr1 aStr2 
					      in  if (aStreamEPEGErrorSseq r aStrRes) then
						      	combinePlusAux aStr1 le1 cl2' (Plus(lExpr2,le1,emptyLustreLab)) r 
					          else
						        combinePlusAux aStr1 le1 cl2' leRes aStrRes ;;
	

let rec combinePlus cl1 cl2 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes)
| (lExpr1,aStr1)::cl1' -> let (lExpr2,aStr2) = combinePlusAux aStr1 lExpr1 cl2 leRes aStrRes 
					      in if (aStreamEPEGErrorSseq aStr2 aStrRes) then
							 	combinePlus cl1' cl2 lExpr2 aStr2 
							 else
								combinePlus cl1' cl2 leRes aStrRes ;;


let rec combineMultAux aStr1 le1 cl2 leRes aStrRes = match cl2 with
  [] -> (leRes,aStrRes)
| (lExpr2,aStr2)::cl2' -> let r = aStreamMult aStr1 aStr2   
			  in if (aStreamEPEGErrorSseq r aStrRes) then
			    combineMultAux aStr1 le1 cl2' (Times(lExpr2,le1,emptyLustreLab)) r 
					          else
						        combineMultAux aStr1 le1 cl2' leRes aStrRes ;;
	

let rec combineMult cl1 cl2 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes)
| (lExpr1,aStr1)::cl1' -> 	let (lExpr2,aStr2) = combineMultAux aStr1 lExpr1 cl2 leRes aStrRes 
					      in if (aStreamEPEGErrorSseq aStr2 aStrRes) then
							 	combineMult cl1' cl2 lExpr2 aStr2 
							 else
								combineMult cl1' cl2 leRes aStrRes ;;


let rec combineUnaryMinus cl1 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes) ;
| (lExpr1,aStr1)::cl1' -> 	let (minusLE1,minusAstr1) = (Uminus(lExpr1,emptyLustreLab),aStreamUMinus aStr1) in
							if (aStreamEPEGErrorSseq minusAstr1 aStrRes) then
							 	combineUnaryMinus cl1' minusLE1 minusAstr1 
							 else
								combineUnaryMinus cl1' leRes aStrRes ;;


let rec combineSqrt cl1 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes) ;
| (lExpr1,aStr1)::cl1' -> 	let (sqrtLE1,sqrtAstr1) = (Sqrt(lExpr1,emptyLustreLab),aStreamSqrt aStr1) in
							if (aStreamEPEGErrorSseq sqrtAstr1 aStrRes) then
							 	combineSqrt cl1' sqrtLE1 sqrtAstr1 
							 else
								combineSqrt cl1' leRes aStrRes ;;


let rec combineCos cl1 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes) ;
| (lExpr1,aStr1)::cl1' -> 	let (cosLE1,cosAstr1) = (Cos(lExpr1,emptyLustreLab),aStreamCos aStr1) in
							if (aStreamEPEGErrorSseq cosAstr1 aStrRes) then
							 	combineCos cl1' cosLE1 cosAstr1 
							 else
								combineCos cl1' leRes aStrRes ;;


let rec combineSin cl1 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes) ;
| (lExpr1,aStr1)::cl1' -> 	let (sinLE1,sinAstr1) = (Sin(lExpr1,emptyLustreLab),aStreamSin aStr1) in
							if (aStreamEPEGErrorSseq sinAstr1 aStrRes) then
							 	combineSin cl1' sinLE1 sinAstr1 
							 else
								combineSin cl1' leRes aStrRes ;;



let rec combineMinusAux aStr1 le1 cl2 leRes aStrRes = match cl2 with
  [] -> (leRes,aStrRes)
| (lExpr2,aStr2)::cl2' -> let r = aStreamMinus aStr1 aStr2 
					      in  if (aStreamEPEGErrorSseq r aStrRes) then
						      	combineMinusAux aStr1 le1 cl2' (Minus(le1,lExpr2,emptyLustreLab)) r 
					          else
						        combineMinusAux aStr1 le1 cl2' leRes aStrRes ;;
	

let rec combineMinus cl1 cl2 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes)
| (lExpr1,aStr1)::cl1' -> let (lExpr2,aStr2) = combineMinusAux aStr1 lExpr1 cl2 leRes aStrRes 
					      in if (aStreamEPEGErrorSseq aStr2 aStrRes) then
							 	combineMinus cl1' cl2 lExpr2 aStr2 
							 else
								combineMinus cl1' cl2 leRes aStrRes ;;


let rec combineDivAux aStr1 le1 cl2 leRes aStrRes = match cl2 with
  [] -> (leRes,aStrRes)
| (lExpr2,aStr2)::cl2' -> let r = aStreamDiv aStr1 aStr2 
					      in  if (aStreamEPEGErrorSseq r aStrRes) then
						      	combineDivAux aStr1 le1 cl2' (Div(le1,lExpr2,emptyLustreLab)) r 
					          else
						        combineDivAux aStr1 le1 cl2' leRes aStrRes ;;
								
								
let rec combineDiv cl1 cl2 leRes aStrRes = match cl1 with
  [] -> (leRes,aStrRes)
| (lExpr1,aStr1)::cl1' -> let (lExpr2,aStr2) = combineDivAux aStr1 lExpr1 cl2 leRes aStrRes 
					      in if (aStreamEPEGErrorSseq aStr2 aStrRes) then
							 	combineDiv cl1' cl2 lExpr2 aStr2 
							 else
								combineDiv cl1' cl2 leRes aStrRes ;;								
					
					 
let rec profiteLustreExpr le lenv = match le with 
		Id(v,_) -> (le,snd (getEnvComplete v lenv (Id("UNDEFINED",emptyLustreLab),asTop)))
	|	Cst(f,_) -> (le,f)
	|	_ -> raise (EPEGError("profiteLustreExpr"," only values should appear in Lustre profitability")) 

	
and profiteAbstractBox ab eqClassBelowList profEqClassBelowList lenv = match ab with
		Box(OP_PLUS,bcList) -> 
			let bcListProfite = List.map (fun x -> profiteBoxContent !x eqClassBelowList profEqClassBelowList lenv) bcList 
		    in reifyPlus bcListProfite 
	| 	Box(OP_MULT,bcList) ->
			let bcListProfite = List.map (fun x -> profiteBoxContent !x eqClassBelowList profEqClassBelowList lenv) bcList 
		    in  reifyMult bcListProfite 
	|       _ -> raise (EPEGError("profiteAbstractBox","impossible case"))

			  
and profiteBoxContent bcList eqClassBelowList profEqClassBelowList lenv = match bcList with
		VLeaf(le) -> profiteLustreExpr le lenv  
    | 	Box_(ab) -> profiteAbstractBox !ab eqClassBelowList profEqClassBelowList lenv  
	|	VNode(n) -> match n with
						PEGNode(op,l) ->  
					  		profitePegNode (ref n) eqClassBelowList profEqClassBelowList  lenv 
					|	_ -> raise(EPEGError("Unexpected node type in the abstract box ","profiteBoxContent"))

									 
and profitePegNode pn eqClassBelowList profEqClassBelowList lenv = match !pn with
	(* finds the best concrete lexpression and related abstract stream *)
		PEGLeaf(le) -> profiteLustreExpr le lenv 
	|	PEGNode(OP_PLUS,pnList) ->  
			(* meaning : operator * [arguments] *)
			let (pn1,pn2) = list2Pair pnList in
	    	let profEqClass1 = 	getEqClassBelow (getEqClass pn1 eqClassBelowList) profEqClassBelowList lenv in
	    	let profEqClass2 = 	getEqClassBelow (getEqClass pn2 eqClassBelowList) profEqClassBelowList lenv in 
		    	combinePlus profEqClass1 profEqClass2 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_MINUS,pnList) ->  
			(* meaning : operator * [arguments] *) 
			let (pn1,pn2) = list2Pair pnList in
	    	let profEqClass1 = 	getEqClassBelow (getEqClass pn1 eqClassBelowList) profEqClassBelowList lenv in
	    	let profEqClass2 = 	getEqClassBelow (getEqClass pn2 eqClassBelowList) profEqClassBelowList lenv in 
	    		combineMinus profEqClass1 profEqClass2 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_UMINUS,pnList) -> 
		(* meaning : operator * [arguments] *) 
			let pn1 = List.hd pnList in
			let eqc = (getEqClass pn1 eqClassBelowList) in
    		let profEqClass1 = flattenEqClassEnv (profiteEqClass (ref eqc) lenv) [] in 
    			combineUnaryMinus profEqClass1 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_SQRT,pnList) -> 
		(* meaning : operator * [arguments] *) 
			let pn1 = List.hd pnList in
			let eqc = (getEqClass pn1 eqClassBelowList) in
    		let profEqClass1 = flattenEqClassEnv (profiteEqClass (ref eqc) lenv) [] in 
    			combineSqrt profEqClass1 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_COS,pnList) -> 
		(* meaning : operator * [arguments] *) 
			let pn1 = List.hd pnList in
			let eqc = (getEqClass pn1 eqClassBelowList) in
    		let profEqClass1 = flattenEqClassEnv (profiteEqClass (ref eqc) lenv) [] in 
    			combineCos profEqClass1 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_SIN,pnList) -> 
		(* meaning : operator * [arguments] *) 
			let pn1 = List.hd pnList in
			let eqc = (getEqClass pn1 eqClassBelowList) in
    		let profEqClass1 = flattenEqClassEnv (profiteEqClass (ref eqc) lenv) [] in 
    			combineSin profEqClass1 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_MULT,pnList) ->  
			(* meaning : operator * [arguments] *) 
			let (pn1,pn2) = list2Pair pnList in  
	    	let profEqClass1 = 	getEqClassBelow (getEqClass pn1 eqClassBelowList) profEqClassBelowList lenv in
	    	let profEqClass2 = 	getEqClassBelow (getEqClass pn2 eqClassBelowList) profEqClassBelowList lenv in
	    		combineMult profEqClass1 profEqClass2 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	PEGNode(OP_DIV,pnList) ->  
			(* meaning : operator * [arguments] *) 
			let (pn1,pn2) = list2Pair pnList in
	    	let profEqClass1 = 	getEqClassBelow (getEqClass pn1 eqClassBelowList) profEqClassBelowList lenv in
	    	let profEqClass2 = 	getEqClassBelow (getEqClass pn2 eqClassBelowList) profEqClassBelowList lenv in 
	    		combineDiv profEqClass1 profEqClass2 (Id("UNDEFINED2",emptyLustreLab)) asTop 
	|	BoxNode(OP_PLUS,ab,eqc) ->  
			(* meaning : operator * a box * an equivalent class *) 
			let (e1,as1) = profiteAbstractBox !ab eqClassBelowList profEqClassBelowList lenv in
			let profEqClass = flattenEqClassEnv (profiteEqClass eqc lenv) [] in
			let res =	combinePlusAux as1 e1 profEqClass (Id("UNDEFINED2",emptyLustreLab)) asTop 
			in res
	|	BoxNode(OP_MULT,ab,eqc) ->  
			(* meaning : operator * a box * an equivalent class *) 
			let (e1,as1) = profiteAbstractBox !ab eqClassBelowList profEqClassBelowList lenv in
			let profEqClass = flattenEqClassEnv (profiteEqClass eqc lenv) [] in
			let res = 
        		combineMultAux as1 e1 profEqClass (Id("UNDEFINED2",emptyLustreLab)) asTop 
            in res
	| 	GlobalBox(ab) -> profiteAbstractBox !ab eqClassBelowList profEqClassBelowList lenv 
	|       _ -> raise (EPEGError("profitePegNode","not yet implemented"))


(* List.map (fun x -> (!x,flattenEqClassEnv (profiteEqClass x lenv) [])) !eqClassBelowList *)
and mapEqClassBelow eqClassBelowList eqClassBelowList2 profBoxesList lenv = match eqClassBelowList with
  [] -> []
| x::xs -> if (isDefEQC x !profiteResEnv) then 
				(x,getProfitEnv x !profiteResEnv)::(mapEqClassBelow xs eqClassBelowList2 profBoxesList lenv)
		   else 	
				let (idRes,valRes) = (x,(flattenEqClassEnv (profiteEqClass x lenv) [])@profBoxesList) in
				let _ = profiteResEnv := (idRes,valRes)::(!profiteResEnv) 
				in (idRes,valRes)::(mapEqClassBelow xs eqClassBelowList2 profBoxesList lenv)


and profiteEqClass eqc lenv = match !eqc with  
	(* returns an env:  pn -> (le,as) *)							  
		None(_) -> [ref (PEGLeaf(Id("UNDEFINED",emptyLustreLab))),(Id("UNDEFINED",emptyLustreLab),asTop)]
	|	Singloton(pn) -> [(pn,profitePegNode pn [] [] lenv)]       
	|	EqClass(pnList,eqClassBelowList,_) -> 
			(* meaning : [nodes inside] * [eq class below] * [boxes below] *)
			let profEqClassBelowList = mapEqClassBelow !eqClassBelowList eqClassBelowList [] lenv
 			in List.map (fun x -> (x,profitePegNode x !eqClassBelowList profEqClassBelowList lenv)) !pnList 


and getEqClassBelow id env lenv = match env with
  [] -> flattenEqClassEnv (profiteEqClass (ref id) lenv) [] 
| (id',v)::env' -> if (eqEqClass id (!id')) then v else (getEqClassBelow id env' lenv) 


let rec profiteLustrePegList pegList env = match pegList with
	[] -> env
| 	(LustrePEG(name,eqc,_))::pegs -> 
        let _ = profiteResEnv := [] in
	let (leRes,asRes) =  selectPN (profiteEqClass eqc env) (Id(name,emptyLustreLab)) asTop 
	in profiteLustrePegList pegs (setEnv name (leRes,asRes) env) ;;


let rec transformInitEnv env = match env with
  [] -> []
| (id,v)::env' -> (id,(Id(id,Lab(0)),v))::(transformInitEnv env') ;;


let analyzeProgram lustrePegList initEnv = 
  let initEnv' = transformInitEnv initEnv 
  in profiteLustrePegList lustrePegList initEnv' ;;





