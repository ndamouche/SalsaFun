
open SalsaTypes ;;
open Epeg_types ;;
open Prelude ;;
open Epeg_prelude ;;

(***********************)
(* OPERATOR PROPERTIES *)
(***********************)

let rec memberPN e l = match l with
  [] -> false
| x::xs -> if (eqPegNode e x) then true else memberPN e xs ;;


let rec memberPN' e l = match l with
  [] -> false
| x::xs -> if (eqPegNode e !x) then true else memberPN' e xs ;;


let rec memberEQC e l = match l with
  [] -> false
| x::xs -> if (eqEqClass e x) then true else memberEQC e xs ;;


let isDistributeGoesThroughtOperator op = match op with
		OP_PLUS -> true
	|	OP_MINUS -> true
	|	OP_UMINUS -> true
	| 	OP_DIV -> true	
	|	_ -> false
;;


let isOperatorSymmetric op = match op with
		OP_PLUS -> true
	|	OP_MULT -> true
	|	_ -> false
;;

let isOperatorDistributable op = match op with
		OP_MULT -> true
	| 	OP_DIV -> false
	|	_ -> false
;;

let isMinusDistributeInAllBranch op = false ;;


(************)
(* ACCESSOR *)
(************)

(* returns the node list from an EqClass *)
let extractNodeListFromEquivalenceClass eqclass = match !eqclass with
		None(_) -> raise(EPEGError("extractNodeListFromEquivalenceClass","None class"))
	|	Singloton(node) -> node::[]
	|	EqClass(list,_,_) -> !list
;;

(* returns the class list from an EqClass *)
let extractEquivalenceClassListFromEquivalenceClass eqclass = match !eqclass with
		None(_) -> raise(EPEGError("extractEquivalenceClassListFromEquivalenceClass","None class"))
	|	Singloton(node) -> eqclass::[]
	|	EqClass(_,list,_) -> !list
;;

(* returns the box list from an EqClass *)
let extractBoxListFromEquivalenceClass eqclass = match !eqclass with
		None(_) -> raise(EPEGError("extractBoxListFromEquivalenceClass","None class"))
	|	Singloton(node) -> raise(EPEGError("extractBoxListFromEquivalenceClass","Can't extract box list from a Singloton."))
	|	EqClass(_,_,al) -> !al
;;

(* returns the list of child nodes of the given node *)
let getChildNodes node = match !node with
		PEGNode(_, l) -> l
	|	PEGLeaf(_) -> []
	|	_ -> raise(EPEGError("getChildNodes","Wrong parameter ! Only PEGNode and PEGLeaf are admitted."))
;;

let getOperatorInNode node = match !node with
		PEGNode(op, _) -> op
	|	_ -> raise(EPEGError("getOperatorInNode","Wrong parameter ! Only PEGNode are admited."))
;;

(* return from the equivalence class list the EqClass that contained the node parameter *)
let rec getEqClassCorresponding pegnode eqclassl = 
	let rec isContained n l = match l with
		[] -> false
	|	t::q -> if (eqPegNode !t !n) then true else (isContained n q)
	in
	match eqclassl with
		[] -> raise(EPEGError("getEqClassCorresponding","No eqClass found containing the pegnode."))
	|	t::q -> if (isContained pegnode (extractNodeListFromEquivalenceClass t)) then t else (getEqClassCorresponding pegnode q)
;;

(*****************)
(* USEFULL STUFF *)
(*****************)

(* in an EqClass the box list is initially created with one dummy element, this function tells if that's the case *)
let checkIfBoxListNeedToBePurged l = match l with
		[a] -> begin match !a with 
				|	Box(OP_PLUS, []) -> true
				|	_ -> false
				end
	|	_ -> false
;;

(* remove a node from a list exactly once *)
let rec removeFromListOnce node list = match list with
		[] -> []
	|	t::q -> if (eqPegNode !t !node) then q else t::(removeFromListOnce node q)
;;

(* create the box content from the PEGLeaf and PEGNode *)
let rec makeBoxContent leaflist = match leaflist with
		[] -> []
	|	t::q -> begin match !t with
					PEGLeaf(le) -> (ref (VLeaf(le)))::(makeBoxContent q)
		
				|	PEGNode(op, lp) -> if (isOperatorSymmetric op) then
											(ref (Box_( ref (Box(op, makeBoxContent lp)) )))::(makeBoxContent q)
										else
											(ref (VNode(!t)))::(makeBoxContent q)
										
				|	_ -> raise(EPEGError("makeBoxContent","At this point there should be only PEGLeaf and PEGNode in the list."))
				end
;;

(* transform a list of referenced object into a list of objects *)
let rec makeExplicitList l = match l with
		[] -> []
	|	t::q -> (!t)::(makeExplicitList q)
;;

(* save a PEGNode or a PEGLeaf in the node list of an EqClass *)
let savePEGNode n eqclass = match !eqclass with
		EqClass(pl,_,_) -> 	let _ = pl := !pl @ [n] in
							()
	|	_ -> raise(EPEGError("savePEGNode","Wrong parameter ! Only EqClass are meant to save PEGNode or PEGLeaf."))
;;

let rec isBoxContainedIn box list = match list with
		[] -> false
	|	t::q -> if (eqPegNode !t !box) then true else (isBoxContainedIn box q)
;;


let rec isBoxContainedIn' box list = match list with
		[] -> false
	|	t::q -> if (eqAbstractBox !t !box) then true else (isBoxContainedIn' box q)
;;

(* save a BoxNode or a GlobalBox and it's box contained into respectively the pegnode list and the box list *)
let saveBoxNode boxnode box eqclass = match !eqclass with
		EqClass(pl,_,bl) -> let _ = if (not (isBoxContainedIn boxnode !pl)) then
										pl := !pl @ [boxnode] 
							in
							let _ = if (not (isBoxContainedIn' box !bl)) then 
										let tmp = if (checkIfBoxListNeedToBePurged !bl) then
												[box]
											else
												(!bl @ [box])
										in bl := tmp
								in ()
	|	_-> raise(EPEGError("saveBoxNode","Wrong parameter ! Only EqClass are meant to save BoxNode or GlobalBox."))
;;

let saveEqClassInClassBelowOfEqClass newclass eqclass = match !eqclass with
		EqClass(_,cb,_) -> 	let _ = cb := !cb @ [newclass] in
							()
	|	_ -> raise(EPEGError("saveEqClassInClassBelowOfEqClass","Wrong parameter ! Only EqClass are admited as second parameter."))
;;

let saveEqClassInLustrePEG newclass lustrepeg = match lustrepeg with
	LustrePEG(_,_,eql) -> let _ = eql := !eql @ [newclass] in
						()
;;


let	rec isNodeAlreadyContained node eqclass = match eqclass with
		EqClass(nodel,_,_) -> let ll = makeExplicitList !nodel in
							  memberPN node ll 
	|	Singloton(nodein) -> eqPegNode node (!nodein)
	|	_ -> false
and
	trySavePEGNode pegnode eqclass =
		if (not(isNodeAlreadyContained !pegnode !eqclass)) then
			savePEGNode pegnode eqclass
and
	trySaveEqClassInClassBelowOfEqClass eq eqclass =
		if (not(isEqClassAlreadyContained !eq !eqclass)) then 
			saveEqClassInClassBelowOfEqClass eq eqclass
and
	trySaveEqClassInClassBelowOfEqClassOn eql eqclass = match eql with
		[] -> []
	|	t::q -> let _ = trySaveEqClassInClassBelowOfEqClass t eqclass in
				trySaveEqClassInClassBelowOfEqClassOn q eqclass
and
	trySaveEqClassInLustrePEG eq lp = match lp with
		LustrePEG(_,_,eql) -> let ll = makeExplicitList !eql in
							if (not(memberEQC !eq ll)) then 
								saveEqClassInLustrePEG eq lp
and
	isEqClassAlreadyContained eq eqclass = match eqclass with
		EqClass(_,eql,_) -> let ll = makeExplicitList !eql in
							  memberEQC eq ll 
	|	Singloton(_) -> eq = eqclass
	|	_ -> false
;;


(**********************)
(* E-PEG CONSTRUCTION *)
(**********************)


(* create the container EqClass for the node, second parameter is the list of equivalence class below *)
let makeEquivalenceClass node eql = match !node with
		PEGLeaf(_) -> ref (Singloton(node))
	|	PEGNode(op, args) -> ref (EqClass( ref (node::[]), 
										   ref eql, 
										   ref ((ref (Box(OP_PLUS, [])))::[])
										 ))
|	_ -> raise(EPEGError("makeEquivalenceClass","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))
;;

let rec isNodeContainedIn node nodel = match nodel with
		[] -> false
	|	t::q -> if (eqPegNode t node) then true else (isNodeContainedIn node q)
;;

let rec getNodeContainedIn node nodel = match nodel with
		[] -> raise(EPEGError("getNodeContainedIn","Element not found !"))
	|	t::q -> if (eqPegNode !t node) then t else (getNodeContainedIn node q)
;;

let rec getAllNodeFromEqClassList eqlist = match eqlist with
		[] -> []
	|	t::q -> (extractNodeListFromEquivalenceClass t) @ (getAllNodeFromEqClassList q)
;;

let rec isEqClassAlreadyContainedBelow eqc eqcsup =
	let below = extractEquivalenceClassListFromEquivalenceClass eqcsup in
	let rec searchEqCIn eqc eql = match eql with
		[] -> false
	|	t::q -> if (eqEqClass !t !eqc) then true else (searchEqCIn eqc q)
	in
		searchEqCIn eqc below
;;



(*********** DISTRIBUTIVITY ***********)

let rec doEPEGConstruction_Distribute rootnode eqclass lp =

	let extractNodesListToDistribute node opnode = getChildNodes node
	in
	match !rootnode with
	|	PEGLeaf(leaf) -> ()
	|	PEGNode(rootop, rootl) -> 
			let opnode = getOperatorInNode rootnode in
			let infnode = extractNodesListToDistribute rootnode opnode in
			let explicitinfnode = makeExplicitList infnode in
			begin match explicitinfnode with
				[] -> ()
			|	[PEGLeaf(a)] -> ()
			|	[PEGLeaf(ex1);PEGLeaf(ex2)] -> ()
			|	[PEGNode(op, l)] -> let node = (List.nth infnode 0) in
					 				let pre_classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
									let pre_nodeclassrelated = getEqClassCorresponding node pre_classbelow in
									
									(* we do the process in a bottom-top way *)
					 				let _ = doEPEGConstruction_Distribute node pre_nodeclassrelated lp in
					 				()

			|	[PEGLeaf(ex1);PEGNode(op2,l2)] -> 
												let leaf = (List.hd infnode) in
								 				let node = (List.nth infnode 1) in
								 				let pre_classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
			 									let pre_nodeclassrelated = getEqClassCorresponding node pre_classbelow in
			 									
								 				(* we do the process in a bottom-top way *)
								 				let _ = doEPEGConstruction_Distribute node pre_nodeclassrelated lp in
								 				
								 				let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
			 									let leafclassrelated = getEqClassCorresponding leaf classbelow in
			 									let nodeclassrelated = getEqClassCorresponding node classbelow in
			 									
			 									let allothernoderelated = extractNodeListFromEquivalenceClass nodeclassrelated in
			 									if (isOperatorDistributable opnode) then
			 										let _ = distributeOn allothernoderelated opnode nodeclassrelated leaf leafclassrelated eqclass lp in
													()
												else
													()
		
			|	[PEGNode(op1,l1);PEGLeaf(ex2)] ->
												let node = (List.hd infnode) in
								 				let leaf = (List.nth infnode 1) in
								 				
								 				let pre_classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
			 									let pre_nodeclassrelated = getEqClassCorresponding node pre_classbelow in
			 									
								 				(* we do the process in a bottom-top way *)
								 				let _ = doEPEGConstruction_Distribute node pre_nodeclassrelated lp in
								 				
								 				let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
			 									let leafclassrelated = getEqClassCorresponding leaf classbelow in
			 									let nodeclassrelated = getEqClassCorresponding node classbelow in
			 									
			 									let allothernoderelated = extractNodeListFromEquivalenceClass nodeclassrelated in
			 									if (isOperatorDistributable opnode) then
													let _ = distributeOn allothernoderelated opnode nodeclassrelated leaf leafclassrelated eqclass lp in
													()
												else
													()
			 							
			|	[PEGNode(op1,l1);PEGNode(op2,l2)] -> 
													let node1 = (List.hd infnode) in
									 				let node2 = (List.nth infnode 1) in
									 				let pre_classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
			 										let pre_node1classrelated = getEqClassCorresponding node1 pre_classbelow in
			 										let pre_node2classrelated = getEqClassCorresponding node2 pre_classbelow in
			 									
									 				(* we do the process in a bottom-top way *)
								 					let _ = doEPEGConstruction_Distribute node1 pre_node1classrelated lp in
								 					let _ = doEPEGConstruction_Distribute node2 pre_node2classrelated lp in
								 				
									 				let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
				 									let node1classrelated = getEqClassCorresponding node1 classbelow in
				 									let node2classrelated = getEqClassCorresponding node2 classbelow in
				 									
				 									let allothernode1related = extractNodeListFromEquivalenceClass node1classrelated in
				 									let allothernode2related = extractNodeListFromEquivalenceClass node2classrelated in
				 									
				 									if (isOperatorDistributable opnode) then
				 										let _ = distributeOn allothernode1related opnode node1classrelated node2 node2classrelated eqclass lp in
														let _ = distributeOn allothernode2related opnode node2classrelated node1 node1classrelated eqclass lp in
														()
													else
														()
			
			|	_ -> raise(EPEGError("doEPEGConstruction_Distribute",("n-ary operators are not handled yet.")))
		end
	| 	_ -> raise(EPEGError("doEPEGConstruction_Distribute","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))
		
and 
	distributeOn nodel distoperator classnode factor factorclass superiorclass lp = match nodel with
			[] -> ()
		|	t::q -> let _ = begin match !t with
					|	PEGNode(op,l) ->
										if (isDistributeGoesThroughtOperator op) then
											let lclass = extractEquivalenceClassListFromEquivalenceClass classnode in
											
											(* we have to be careful and remove the conditional part of the ifthenelse *)
											let list = if (op == OP_DIV) then [(List.nth l 0)] else l in
											
											let mult = applyDistributeOnNodeList list lclass factor factorclass superiorclass lp in
											
											(* then we need to do 2 things, put the condition on the eqclass below *)
											(* and put it back in the list in order to make the new node ifthenelse *)
											let mult' = 
														if (op == OP_DIV) then 
															let denom = List.nth l 1 in
															let denomc = getEqClassCorresponding denom (extractEquivalenceClassListFromEquivalenceClass classnode) in
															let _ = trySaveEqClassInClassBelowOfEqClass denomc superiorclass in
															mult@[denom]
														else 
															mult 
											in
											
											if ((List.length mult)>0) then
												let newnode = ref (PEGNode(op, mult')) in
												let _ = trySavePEGNode newnode superiorclass in
												doEPEGConstruction_Distribute newnode superiorclass lp 
					|	PEGLeaf(_) -> ()
					|	_ -> ()
					end
					in
					distributeOn q distoperator classnode factor factorclass superiorclass lp
and
	applyDistributeOnNodeList nodel nodelclass factor factorclass superiorclass lp = match nodel with
		[] -> []
	|	t::q -> let allnode = getAllNodeFromEqClassList (extractEquivalenceClassListFromEquivalenceClass superiorclass) in
				let explallnode = makeExplicitList allnode in
				let newnode = PEGNode(OP_MULT, [factor;t]) in
				if (isNodeContainedIn newnode explallnode) then
					let nold = getNodeContainedIn newnode allnode in
					nold :: applyDistributeOnNodeList q nodelclass factor factorclass superiorclass lp
				else
					let nn = (ref newnode) in
					let nodeclassrelated = getEqClassCorresponding t nodelclass in
					let classbelow = [nodeclassrelated ; factorclass] in
					let nc = makeEquivalenceClass nn classbelow in
					let _ = trySaveEqClassInClassBelowOfEqClass nc superiorclass in
					let _ = trySaveEqClassInLustrePEG nc lp in
					nn::(applyDistributeOnNodeList q nodelclass factor factorclass superiorclass lp)
;;


(*********** FACTORIZATION ***********)


let isOperatorFactorizable op = match op with
		OP_MULT -> true
	| OP_DIV -> true
	|	_ -> false
;;

let isFactorizationGoesThrough op = match op with
		OP_MULT -> true
	| OP_DIV -> true
	| OP_PLUS -> true
	| OP_MINUS -> true
	|	OP_UMINUS -> true
	| _ -> false
;;

let rec findCommonFactorOn l1 l2 = match l1 with
		[] -> []
	|	t::q -> let found = findCommonFactor t l2 in
				if (found) then
					let l2cleaned = removeElementFromList t l2 in
					t::(findCommonFactorOn q l2cleaned)
				else
					findCommonFactorOn q l2
and
	findCommonFactor e l2 = match l2 with
		[] -> false
	|	t::q -> begin match [e;t] with 
				|	[(a,b);(c,d)] -> if (eqPegNode !a !c) then true else (findCommonFactor e q)
				|	_ -> raise(EPEGError("findCommonFactor","Invalid State ! Couldn't have no argument."))
				end
and
	removeElementFromList (el,v) l = match l with
		[] -> []
	|	(t,v')::q -> if (eqPegNode !t !el) then q else (t,v')::(removeElementFromList (el,v) q)
;;


(* This function make a tree from the given list of nodes and a single operator *)
(* that will be repeated all over the structure *)

let rec makeSimpleTree op leafl eqclasssup lp = match leafl with
		[(a,ca)] -> let _ = if (not (isEqClassAlreadyContainedBelow ca eqclasssup)) then 
								saveEqClassInClassBelowOfEqClass ca eqclasssup	
					in
					(a,ca)
	|	[(a,ca);(b,cb)] -> 
				let n = ref (PEGNode(op, [a;b]))in
				let cn = makeEquivalenceClass n [ca;cb] in
				let _ = trySaveEqClassInClassBelowOfEqClass cn eqclasssup in 
				let _ = trySaveEqClassInLustrePEG cn lp in
				(n,cn)
					
	|	(t,ct)::q -> (* we create an empty class to give to the inferior class, it will be fill when all the rest is done *)
					let emptyc = ref(EqClass(ref [], ref [], ref ((ref (Box(OP_PLUS, [])))::[]))) in
					let rest = makeSimpleTree op q emptyc lp in
					begin match rest with
						(ninf,cinf) ->
								let n = ref (PEGNode(op, [t;ninf]))in
								let _ = trySavePEGNode n emptyc in
								let _ = trySaveEqClassInClassBelowOfEqClass ct emptyc in 
								let _ = trySaveEqClassInClassBelowOfEqClass emptyc eqclasssup in 
								let _ = trySaveEqClassInLustrePEG emptyc lp in
								(n,emptyc)
					end
	|	_ -> raise(EPEGError("makeSimpleTree","This function can't make an empty tree, list should be of size at least one."))
;;



let rec removeFactorIn factor common = match common with
	[] -> raise(EPEGError("removeFactorIn","Factor can not be found !"))
|	(f,c)::q -> if (eqPegNode !factor !f) then q else (f,c)::(removeFactorIn factor q)
;;

let getFirst triplet = match triplet with
	(a,b,c) -> a
(*|	_ -> raise(EPEGError("getFirst","Wrong parameter ! need a 3-uplet."))*)
;;
let getSecond triplet = match triplet with
	(a,b,c) -> b
(*|	_ -> raise(EPEGError("getSecond","Wrong parameter ! need a 3-uplet."))*)
;;
let getThird triplet = match triplet with
	(a,b,c) -> c
(*|	_ -> raise(EPEGError("getThird","Wrong parameter ! need a 3-uplet."))*)
;;

let rec getMultNodeIn nodelist nb = match nodelist with
	[] -> -1
|	t::q -> begin match !t with
			|	PEGNode(OP_MULT, _) -> nb
			|	_ -> getMultNodeIn q (nb+1)
			end
;;

let rec invertedFactors factors = match factors with
	| [] -> []
	| (node,eq)::q -> 
			begin match !node with
			| PEGNode(OP_DIV, [n;d]) -> 
				begin match !n with
				|  PEGLeaf(Cst(value,lab)) ->
						if (isOne (Cst(value,lab))) then
							let classbelow = extractEquivalenceClassListFromEquivalenceClass eq in
		 					let dclassrelated = getEqClassCorresponding d classbelow in
							(d,dclassrelated)::(invertedFactors q)
						else
							 (invertedFactors q)
				| _ -> (invertedFactors q)
				end
			| _ -> (invertedFactors q)
			end
;;

(* This function duplicate a tree and remove all occurrence of the common factors *)
(* it then add the common factors to the structure as the result of the maximal factorization *)	
let rec cleanUpTree	tree common needtofind eqclass lp = match !tree with
	PEGNode(op, l)->
if (memberPN' !tree (List.map fst common)) then
						(* if the factor is a whole subtree we could replace it by 1 *)
						let un = ref (PEGLeaf(one)) in
						let unc = makeEquivalenceClass un [] in
						let _ = trySaveEqClassInLustrePEG unc lp in
						let common' = removeFactorIn tree common in
						(un,unc,common')
	
					else if (isOperatorFactorizable op) then
						(* we are dealing with something like an OP_MULT operator, we have to factorize once *)
						
							let l = cleanUpTreeOn op l common true false eqclass lp in
							let nodes = List.map getFirst l in
							let classes = List.map getSecond l in
							let commons = List.map getThird l in
							let lastcommon = (List.nth commons ((List.length commons)-1)) in
							
							let newnode = ref (PEGNode(op, nodes)) in
							let newclass = makeEquivalenceClass newnode classes in 
							let _ = trySaveEqClassInLustrePEG newclass lp in
							
							(newnode, newclass, lastcommon)
						
					else
						(* we are dealing with something like an OP_PLUS operator, we factorize in both branches *)
						let l = cleanUpTreeOn op l common false needtofind eqclass lp in
						let nodes = List.map getFirst l in
						let classes = List.map getSecond l in
						let commons = List.map getThird l in
						let isempty = (fun x -> x = []) in
						
						(* if we have factorize in all branches maybe it's because we have to factorize in an other node *)
						(* like a MULT node we have created before, if there isn't one, then the process fails *)
						if (not (List.for_all isempty commons)) then
						
							let others = extractNodeListFromEquivalenceClass eqclass in
							let multidx = getMultNodeIn others 0 in
							if (((List.length others) > 1) && (not (multidx = -1))) then
								let mul = List.nth others multidx in
								begin match !mul with 
								| PEGNode(OP_MULT, ll) ->
									let lm = cleanUpTreeOn OP_MULT ll common false true eqclass lp in
									let nodesm = List.map getFirst lm in
									let classesm = List.map getSecond lm in
									let commonsm = List.map getThird lm in
									let lastcommonm = (List.nth commonsm ((List.length commonsm)-1)) in
									
									let newnodem = ref (PEGNode(OP_MULT, nodesm)) in
									let newclassm = makeEquivalenceClass newnodem classesm in 
									let _ = trySaveEqClassInLustrePEG newclassm lp in
									(newnodem, newclassm, lastcommonm)
								| _ -> raise(EPEGError("cleanUpTree","Impossible case, the selected node must be of type OP_MULT")) 
								end
							else
								if (needtofind) then
									raise(EPEGError("cleanUpTree","Factor haven't not been all founded in all branches !")) 
								else
									(tree, eqclass, common)
						else
							let newnode = ref (PEGNode(op, nodes)) in
							let newclass = makeEquivalenceClass newnode classes in 
							let _ = trySaveEqClassInLustrePEG newclass lp in
							(newnode, newclass, [])
	
|	PEGLeaf(ex) -> if (memberPN' !tree (List.map fst common)) then
						(* we found one of the factor searched *)
						let un = ref (PEGLeaf(one)) in
						let unc = makeEquivalenceClass un [] in
						let _ = trySaveEqClassInLustrePEG unc lp in
						let common' = removeFactorIn tree common in
						(un,unc,common')
					else
						(* if we don't find the factor on a leaf we probably are in a OP_MULT kind of branch *)
						(* if we not in such a case then the process will fail on the superior operator *)
						(tree, eqclass, common)

| _ -> raise(EPEGError("cleanTreeOfLeaves","Tree should only contained PEGNode and PEGLeaf."))

and
	cleanUpTreeOn op ltree common propagatecommon needtofind eqclass lp = match ltree with
		[] -> []
	|	t::q -> let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
	 			let classrelated = getEqClassCorresponding t classbelow in
	 			if (propagatecommon) then
					(* if it's a division we have to be cautious *)
					if (op = OP_DIV) then
						let num = t in
						let denum = (List.nth q 0) in
						let denum_classrelated = getEqClassCorresponding denum classbelow in
						(* we first search for common factor on the numerators*)
						let (t,c,common') = (cleanUpTree num common needtofind classrelated lp) in
						(* then we search for the inverted factors like 1/a in the denominator *)
						[(t,c,common') ; (cleanUpTree denum (invertedFactors common') needtofind denum_classrelated lp)]
					else
		 				let (t,c,common') = (cleanUpTree t common needtofind classrelated lp) in
		 				(t,c,common')::(cleanUpTreeOn op q common' propagatecommon needtofind eqclass lp)
	 			else
					(cleanUpTree t common needtofind classrelated lp)::(cleanUpTreeOn op q common propagatecommon needtofind eqclass lp)
;;


let rec prepareDenominatorForFactorization denomfactor eqclasssup = match denomfactor with
	| [] -> []
	| (node,eq)::q -> 
						let un = ref (PEGLeaf(one)) in
						let unc = makeEquivalenceClass un [] in
						let node = ref (PEGNode(OP_DIV,[un;node])) in
						let nodeeq = makeEquivalenceClass node [unc; eq] in
						let _ = trySaveEqClassInClassBelowOfEqClass nodeeq eqclasssup in 
						(node,nodeeq) :: (prepareDenominatorForFactorization q eqclasssup)
;;


let rec doEPEGConstruction_Factorize rootnode eqclass lp =
	match !rootnode with
	|	PEGLeaf(a) -> [(rootnode,eqclass)]
	|	PEGNode(op,l) ->
						let factnodes = 
							if (op = OP_DIV) then
							(* for the division we performed separately numerator and denominator *)
								let num = (List.nth l 0) in
								let denom = (List.nth l 1) in
								
								let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
								
								let numclass = getEqClassCorresponding num classbelow in
								let num_f = doEPEGConstruction_Factorize num numclass lp in
								
								let denomclass = getEqClassCorresponding denom classbelow in
								let denom_f = doEPEGConstruction_Factorize denom denomclass lp in
								
								(* we invert the factor on the denominator as if they were 1/fd1 .. 1/fdl *)
								let denom_finv = prepareDenominatorForFactorization denom_f eqclass in
								
								(* we group both facto fn1..fnk(1/fd1)...(1/fdl) as is they were linked by a multiply *)
								[num_f ; denom_finv]
							
							else
								doEPEGConstruction_FactorizeOn op l eqclass lp
						in
												
						if (isOperatorFactorizable op) then
							List.flatten factnodes
						else
							if (not(isFactorizationGoesThrough op)) then
								[(rootnode,eqclass)]
							else
								begin match factnodes with
								|	[a] -> a
											
								|	[a;b] -> let commonfactor = findCommonFactorOn a b in
											
											 if (commonfactor = []) then
											 	[(rootnode,eqclass)]
											 else
											 	
												 let new_tree = cleanUpTree rootnode commonfactor true eqclass lp in
												 let newnode = getFirst new_tree in
												 let newclass = getSecond new_tree in
											
												 let multtree = makeSimpleTree OP_MULT commonfactor eqclass lp in
												 let multnode = fst multtree in
												 let multclass = snd multtree in
												
												 let topnode = ref (PEGNode(OP_MULT, [multnode ; newnode])) in
												 let _ = trySavePEGNode topnode eqclass in
												
												 let topclass = makeEquivalenceClass topnode [multclass ; newclass] in
												 
												 let _ = trySaveEqClassInLustrePEG topclass lp in
												 let _ = trySaveEqClassInClassBelowOfEqClass newclass eqclass in
												 let _ = trySaveEqClassInClassBelowOfEqClass multclass eqclass in
												 
												 let out_tree = (getFirst new_tree, getSecond new_tree) in
												 out_tree::commonfactor
												
								|	_ -> raise(EPEGError("doEPEGConstruction_Factorize","n-ary operators are not handled yet."))
								end
							
					
	| 	_ -> raise(EPEGError("doEPEGConstruction_Factorize","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))
and
	doEPEGConstruction_FactorizeOn op nodel eqclass lp = match nodel with
		[a] -> 
				let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
				let nodeaclassrelated = getEqClassCorresponding a classbelow in
				let n1 = doEPEGConstruction_Factorize a nodeaclassrelated lp in
				[n1]
	|	[a;b] -> 
 				let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
				let nodeaclassrelated = getEqClassCorresponding a classbelow in
				let nodebclassrelated = getEqClassCorresponding b classbelow in
				
				let n1 = doEPEGConstruction_Factorize a nodeaclassrelated lp in
				let n2 = doEPEGConstruction_Factorize b nodebclassrelated lp in
				
				[n1;n2]
	|	_ -> raise(EPEGError("doEPEGConstruction_FactorizeOn","n-ary operators are not handled yet."))
;;



(*********** MINUS PROPAGATION ***********)

let rec doEPEGConstruction_MinusPropag rootnode eqclass lp = 
	let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
	match !rootnode with
		PEGLeaf(leaf) -> ()
	|	PEGNode(OP_UMINUS, [a]) ->
							begin match !a with
							(* we don't propagate on simple leaf preceded by a minus operator *)
							|	PEGLeaf(ex) -> () 
							(* however we propagate on every thing else *)
							|	PEGNode(_,_) -> let aeqclass = getEqClassCorresponding a eqclassbelow in
												let (ret,_) = propagateMinusDown a aeqclass eqclass OP_UMINUS lp in
												
												let _ = if (not(isNodeAlreadyContained !ret !eqclass)) then (savePEGNode ret eqclass) in
												
												(* then we explore the new branch and the old one *)
												let _ = doEPEGConstruction_MinusPropag ret eqclass lp in
												let _ = doEPEGConstruction_MinusPropag a aeqclass lp in
												()
							| _ -> raise(EPEGError("doEPEGConstruction_MinusPropag","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))
							end
							
	|	PEGNode(OP_MINUS, [a;b]) -> 
						let aeqclass = getEqClassCorresponding a eqclassbelow in
									let beqclass = getEqClassCorresponding b eqclassbelow in
									
									let (newnodeb,neweqclassb) = propagateMinusDown b beqclass eqclass OP_MINUS lp in
									
									let newnode = ref(PEGNode(OP_PLUS,[a;newnodeb])) in
									let _ = if (not(isNodeAlreadyContained !newnode !eqclass)) then (savePEGNode newnode eqclass) in
									
									(* then we explore the new branch and the rest of the branches to propagate further minus operators *)
									let _ = doEPEGConstruction_MinusPropag newnode eqclass lp in
									let _ = doEPEGConstruction_MinusPropag a aeqclass lp in
									let _ = doEPEGConstruction_MinusPropag b beqclass lp in
									()
									
	|	PEGNode(op, l) -> 	(* we explore the structure looking for minus operators *)
							let _ = doEPEGConstruction_MinusPropagOn l eqclassbelow lp in
							()
							
	| 	_ -> raise(EPEGError("doEPEGConstruction_MinusPropag","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))


and
	propagateMinusDown node eqclass eqclasssup oplauncher lp = match !node with
		PEGLeaf(a) -> 
					let newnode = ref (PEGNode(OP_UMINUS,[node])) in
					let neweqclass = makeEquivalenceClass newnode [eqclass] in
					let _ = trySaveEqClassInLustrePEG neweqclass lp in
					let _ = if (oplauncher = OP_MINUS) then
								trySaveEqClassInClassBelowOfEqClass neweqclass eqclasssup in
					(newnode,neweqclass)
	|	PEGNode(OP_UMINUS, [a]) ->  let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
									let aeqclass = getEqClassCorresponding a eqclassbelow in
									let _ = begin match !aeqclass with
										|	EqClass(_,list,_) -> if (oplauncher = OP_UMINUS) then
																	saveAllEqClassBelowIn !list eqclasssup
																 else
																 	if (oplauncher = OP_MINUS) then
																 		trySaveEqClassInClassBelowOfEqClass aeqclass eqclasssup
																 else
																 	raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
										|	Singloton(_) -> trySaveEqClassInClassBelowOfEqClass aeqclass eqclasssup 
										|	_ -> ()
										end
									in
									(a, eqclasssup)
									
	|	PEGNode(OP_MINUS, [a;b]) -> let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
									let aeqclass = getEqClassCorresponding a eqclassbelow in
									let beqclass = getEqClassCorresponding b eqclassbelow in
									
									let (newnodea,newaeqclass) = propagateMinusDown a aeqclass eqclass OP_MINUS lp in
									
									let newnode = ref (PEGNode(OP_PLUS,[newnodea;b])) in
									
									if (oplauncher = OP_MINUS) then
										let neweqclass = makeEquivalenceClass newnode [newaeqclass;beqclass] in
										let _ = trySaveEqClassInLustrePEG neweqclass lp in
										let _ = trySaveEqClassInClassBelowOfEqClass neweqclass eqclasssup in
										(newnode,neweqclass)
									
									else if (oplauncher = OP_UMINUS) then
										let _ = trySaveEqClassInClassBelowOfEqClass newaeqclass eqclasssup in
										let _ = trySaveEqClassInClassBelowOfEqClass beqclass eqclasssup in
										(newnode,eqclasssup)
									else
										raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
	
	|	PEGNode(OP_PLUS, [a;b]) ->
								let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
								let aeqclass = getEqClassCorresponding a eqclassbelow in
								let beqclass = getEqClassCorresponding b eqclassbelow in
								
								if (oplauncher = OP_UMINUS) then
									let newa = ref(PEGNode(OP_UMINUS, [a])) in
									let newaeqclass = makeEquivalenceClass newa [aeqclass] in
									let _ = trySaveEqClassInLustrePEG newaeqclass lp in
									
									let newminus = ref(PEGNode(OP_MINUS, [newa;b])) in
									let _ = trySaveEqClassInClassBelowOfEqClass newaeqclass eqclasssup in
									let _ = trySaveEqClassInClassBelowOfEqClass beqclass eqclasssup in
									(newminus,eqclasssup)
								
								else if (oplauncher = OP_MINUS) then
									let newminus = ref(PEGNode(OP_UMINUS, [node])) in
									let newminuseqclass = makeEquivalenceClass newminus [eqclass] in
									let _ = trySaveEqClassInLustrePEG newminuseqclass lp in
									let _ = trySaveEqClassInClassBelowOfEqClass newminuseqclass eqclasssup in
									(newminus,newminuseqclass)
									
								else
								 	raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
							
				
	|	PEGNode(op, l) ->	(* we dealing with an operator where minus distribute in all branches *)
							if (isMinusDistributeInAllBranch op) then
								let npropag = l in
								let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
								begin match npropag with
								|	[a] ->	let aeqclass = getEqClassCorresponding a eqclassbelow in
											let newa = ref(PEGNode(OP_UMINUS, [a])) in
											let newaeqclass = makeEquivalenceClass newa [aeqclass] in
											let _ = trySaveEqClassInLustrePEG newaeqclass lp in
											
											let newnode = ref(PEGNode(op,[newa])) in
											
											if (oplauncher = OP_MINUS) then
												let neweqclass = makeEquivalenceClass newnode [newaeqclass] in
												let _ = trySaveEqClassInLustrePEG neweqclass lp in
												let _ = trySaveEqClassInClassBelowOfEqClass neweqclass eqclasssup in
												(newnode,neweqclass)
											else if (oplauncher = OP_UMINUS) then
												let _ = trySavePEGNode newnode eqclasssup in
												let _ = trySaveEqClassInClassBelowOfEqClass newaeqclass eqclasssup in
												(newnode,eqclasssup)
											else
												raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
											
								|	[a;b]-> let aeqclass = getEqClassCorresponding a eqclassbelow in
											let beqclass = getEqClassCorresponding b eqclassbelow in
											
											let newa = ref(PEGNode(OP_UMINUS, [a])) in
											let newaeqclass = makeEquivalenceClass newa [aeqclass] in
											let _ = trySaveEqClassInLustrePEG newaeqclass lp in
											
											let newb = ref(PEGNode(OP_UMINUS, [b])) in
											let newbeqclass = makeEquivalenceClass newb [beqclass] in
											let _ = trySaveEqClassInLustrePEG newbeqclass lp in
											
											let nodesprop = [newa;newb] in
											let eqclassprop = [newaeqclass;newbeqclass] in
											let newnode = ref(PEGNode(op,nodesprop)) in
											
											if (oplauncher = OP_MINUS) then
												let neweqclass = makeEquivalenceClass newnode eqclassprop in
												let _ = trySaveEqClassInLustrePEG neweqclass lp in
												let _ = trySaveEqClassInClassBelowOfEqClass neweqclass eqclasssup in
												(newnode,neweqclass)
											else if (oplauncher = OP_UMINUS) then
												let _ = trySavePEGNode newnode eqclasssup in
												let _ = trySaveEqClassInClassBelowOfEqClassOn eqclassprop eqclasssup in
												(newnode,eqclasssup)
											else 
												raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
											
								|	_ -> raise(EPEGError("propagateMinusDown","Invalid number of parameters, should be one or two parameter to propagate within."))
								end
							
							(* else, if we dealing with a MULT or DIV operator for example, we distribute in two different ways *)
							else
								begin match l with
								|	[a;b]-> let eqclassbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
											let aeqclass = getEqClassCorresponding a eqclassbelow in
											let beqclass = getEqClassCorresponding b eqclassbelow in
											
											let newa = ref(PEGNode(OP_UMINUS, [a])) in
											let newaeqclass = makeEquivalenceClass newa [aeqclass] in
											let _ = trySaveEqClassInLustrePEG newaeqclass lp in
											
											let newb = ref(PEGNode(OP_UMINUS, [b])) in
											let newbeqclass = makeEquivalenceClass newb [beqclass] in
											let _ = trySaveEqClassInLustrePEG newbeqclass lp in
											
											let newnode1 = ref(PEGNode(op, [newa;b])) in
											let newnode2 = ref(PEGNode(op, [a;newb])) in
											
											if (oplauncher = OP_MINUS) then
												let neweqclass = makeEquivalenceClass newnode1 [aeqclass;beqclass;newaeqclass;newbeqclass] in
												let _ = trySavePEGNode newnode2 neweqclass in
												let _ = trySaveEqClassInClassBelowOfEqClass neweqclass eqclasssup in
												
												(* we have to propagate manually on the second branch as we only return the first one *)
												let _ = doEPEGConstruction_MinusPropag newnode2 neweqclass lp in
												
												(newnode1,neweqclass)
												
											else if (oplauncher = OP_UMINUS) then
												let _ = trySavePEGNode newnode1 eqclasssup in
												let _ = trySavePEGNode newnode2 eqclasssup in
												let _ = trySaveEqClassInClassBelowOfEqClassOn [aeqclass;beqclass;newaeqclass;newbeqclass] eqclasssup in
												
												(* we have to propagate manually on the second branch as we only return the first one *)
												let _ = doEPEGConstruction_MinusPropag newnode2 eqclasssup lp in
												
												(newnode1,eqclasssup)
											else 
												raise(EPEGError("propagateMinusDown","Invalid operator in parameter should be MINUS or UMINUS."))
											
											
											
								|	_ -> raise(EPEGError("propagateMinusDown","Invalid number of parameters, should be two parameters to propagate in either the first or the second branch."))
								end
					
	| 	_ -> raise(EPEGError("propagateMinusDown","Wrong Argument ! Only PEGNode and PEGLeaf are admited."))
and
	doEPEGConstruction_MinusPropagOn nodel eqclassbelow lp = match nodel with
		[] -> ()
	|	t::q -> 
let teqclass = getEqClassCorresponding t eqclassbelow in
				let _ = doEPEGConstruction_MinusPropag t teqclass lp in
				doEPEGConstruction_MinusPropagOn q eqclassbelow lp
and
	saveAllEqClassBelowIn eqclassl eq = match eqclassl with
		[] -> ()
	|	t::q -> let _ = trySaveEqClassInClassBelowOfEqClass t eq in
				saveAllEqClassBelowIn q eq 
;;



(*********** ACCESS POINT ***********)

let rec applyEPEGConstruction varname node eqclass lp =
	(*let _ = doLoopUnrolling node eqclass varname eqclass !loop_unrolling_factor lp in*)
	let _ = doEPEGConstruction_MinusPropag node eqclass lp in 
	let _ = if (!use_factorize_algorithm) then let _ = doEPEGConstruction_Factorize node eqclass lp in () in
	let _ = if (!use_distribute_algorithm) then doEPEGConstruction_Distribute node eqclass lp in
	()
and 
	applyEPEGConstructionOn varname nodel eqclass lp = match nodel with
		[] -> ()
	|	t::q -> let _ = applyEPEGConstruction varname t eqclass lp in
				applyEPEGConstructionOn varname q eqclass lp
;;

let rec doEPEGConstructionOn lustrepegl = match lustrepegl with
		[] -> []
	|	t::q -> begin match t with
				|	LustrePEG(name,eq,eql) -> 
						let nodel = extractNodeListFromEquivalenceClass eq in
						let _ = applyEPEGConstructionOn name nodel eq t in
						doEPEGConstructionOn q 
				end
;;
