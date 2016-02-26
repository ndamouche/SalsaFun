
open Epeg_types ;;
open Epeg_prelude;;
open Peg2epeg ;;

(**************************)
(* Left-Right Abstraction *)
(**************************)


(* this function creates the boxes by propagating the leaves through the homogeneous areas *)
let rec parcoursPEGHorizontalAbstraction pegnode eqclass = 
	match !pegnode with
		PEGLeaf(expr) -> pegnode::[]
	|	BoxNode(op, ab, eq) -> []
	|	GlobalBox(box) -> []	
	|	PEGNode(op,l) -> 		let explicitl = makeExplicitList l in 
						begin match explicitl with
						|	[a] ->  (* meaning it's a UMINUS, a PRE, or a CURRENT *)
									let node = (List.nth l 0) in
						 			let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
 									let nodeclassrelated = getEqClassCorresponding node classbelow in
 									let _ = parcoursPEGHorizontalAbstraction node nodeclassrelated in
 									[pegnode]
						|	[PEGLeaf(v1) ; PEGLeaf(v2)] ->  
						 								let leaf1 = (List.hd l) in
					 									let leaf2 = (List.nth l 1) in
					 									if (isOperatorSymmetric op) then
						 									(* we could make a local GlobalBox because it's obviously homogeneous *)
					 										let gboxcontent = makeBoxContent ([leaf1;leaf2]) in
					 										let gbox = ref (Box(op, gboxcontent)) in
					 										let globox = ref (GlobalBox(gbox)) in
					 										let _ = saveBoxNode globox gbox eqclass in
						 									[leaf1 ; leaf2]
						 								else
						 									[pegnode]

						|	[PEGLeaf(v1) ; PEGNode(op2,ll)] -> 
															let leaf = (List.hd l) in
						 									let node = (List.nth l 1) in
						 									let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in 
						 									let leafclassrelated = getEqClassCorresponding leaf classbelow in 
						 									
						 									let leafnodelist = parcoursPEGHorizontalAbstraction node nodeclassrelated in 
						 									
														 	(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
						 									let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in 
						 									let cleanother = removeFromListOnce node othernode in 
						 									let _ = applyHorizontalAbstraction cleanother nodeclassrelated in 
						 									
						 									
						 									if (isOperatorSymmetric op) then
							 									let boxcontent = makeBoxContent leafnodelist in 
							 									(* we do not create a box until it's for an homogeneous zone *)
							 									let _ = if ((List.length boxcontent) > 1) then	
									 								let box = ref (Box(op2, boxcontent)) in 
								 									let boxn = ref (BoxNode(op, box, leafclassrelated)) in 
								 									let _ = saveBoxNode boxn box eqclass in 
						 											()
						 										in
						 										(* if we are still in homogeneous area then we propagate all leaves *)
							 									if (op = op2) then
							 										(* we could make a local GlobalBox because it's homogeneous *)
							 										let gboxcontent = makeBoxContent (leafnodelist @ [leaf]) in 
							 										let gbox = ref (Box(op, gboxcontent)) in 
							 										let globox = ref (GlobalBox(gbox)) in 
							 										let _ = saveBoxNode globox gbox eqclass in 
							 										leaf::leafnodelist
							 									(* otherwise we only propagate these two nodes *)
							 									else 
							 										[leaf ; node]
							 								else
							 									[pegnode]
						 									
						|	[PEGNode(op1,ll) ; PEGLeaf(v2)] -> 
						 									let node = (List.hd l) in
						 									let leaf = (List.nth l 1) in
						 									let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in
						 									let leafclassrelated = getEqClassCorresponding leaf classbelow in
						 									
						 									let leafnodelist = parcoursPEGHorizontalAbstraction node nodeclassrelated in
						 									(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
						 									let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
						 									let cleanother = removeFromListOnce node othernode in
						 									let _ = applyHorizontalAbstraction cleanother nodeclassrelated in
						 									
						 									if (isOperatorSymmetric op) then
							 									let boxcontent = makeBoxContent leafnodelist in
							 									(* we do not create a box until it's for an homogeneous zone *)
							 									let _ = if ((List.length boxcontent) > 1) then	
								 									let box = ref (Box(op1, boxcontent)) in
								 									let boxn = ref (BoxNode(op, box, leafclassrelated)) in
								 									let _ = saveBoxNode boxn box eqclass in
							 										()
							 									in
							 									(* if we are still in homogeneous area then we propagate all leaves *)
							 									if (op = op1) then
							 										(* we could make a local GlobalBox because it's homogeneous *)
							 										let gboxcontent = makeBoxContent (leafnodelist @ [leaf]) in
							 										let gbox = ref (Box(op, gboxcontent)) in
							 										let globox = ref (GlobalBox(gbox)) in
							 										let _ = saveBoxNode globox gbox eqclass in
							 										leaf::leafnodelist
							 									(* otherwise we only propagate these two nodes *)
							 									else
							 										[leaf ; node]
							 								else
							 									[pegnode]
						 									
						| 	[PEGNode(op1,l1) ; PEGNode(op2,l2)] -> 
																let node1 = (List.hd l) in
						 										let node2 = (List.nth l 1) in
																let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 										let nodeclassrelated1 = getEqClassCorresponding node1 classbelow in
						 										let nodeclassrelated2 = getEqClassCorresponding node2 classbelow in
						 									
						 										let leafnodelist1 = parcoursPEGHorizontalAbstraction node1 nodeclassrelated1 in
						 										let leafnodelist2 = parcoursPEGHorizontalAbstraction node2 nodeclassrelated2 in
						 										(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode1 = (extractNodeListFromEquivalenceClass nodeclassrelated1) in
							 									let cleanother1 = removeFromListOnce node1 othernode1 in
							 									let _ = applyHorizontalAbstraction cleanother1 nodeclassrelated1 in
							 									
							 									(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode2 = (extractNodeListFromEquivalenceClass nodeclassrelated2) in
							 									let cleanother2 = removeFromListOnce node2 othernode2 in
							 									let _ = applyHorizontalAbstraction cleanother2 nodeclassrelated2 in
							 									
							 									if (isOperatorSymmetric op) then
							 										let boxcontent1 = makeBoxContent leafnodelist1 in
							 										let boxcontent2 = makeBoxContent leafnodelist2 in
							 										(* we do not create a box until it's for an homogeneous zone *)
							 										let _ = if ((List.length boxcontent1) > 1) then
								 										let box1 = ref (Box(op1, boxcontent1)) in
								 										let boxn1 = ref (BoxNode(op, box1, nodeclassrelated2)) in
								 										let _ = saveBoxNode boxn1 box1 eqclass in
								 										()
								 									in
								 									(* we do not create a box until it's for an homogeneous zone *)
								 									let _ = if ((List.length boxcontent2) > 1) then
								 										let box2 = ref (Box(op2, boxcontent2)) in
								 										let boxn2 = ref (BoxNode(op, box2, nodeclassrelated1)) in
								 										let _ = saveBoxNode boxn2 box2 eqclass in
								 										()
								 									in
								 									(* if we are still in homogeneous area then we propagate all leaves *)
							 										if ((op = op1) && (op = op2)) then
							 											(* we could make a local GlobalBox because it's homogeneous *)
							 											let gboxcontent = makeBoxContent (leafnodelist1 @ leafnodelist2) in
								 										let gbox = ref (Box(op, gboxcontent)) in
								 										let globox = ref (GlobalBox(gbox)) in
							 											let _ = saveBoxNode globox gbox eqclass in
							 											leafnodelist1 @ leafnodelist2
							 										else
							 											(* if the first son is homogenous then we propagate it's leaves with the other node *)
							 											if ((op = op1) && (not (op = op2))) then
							 												(* we could make a partial local GlobalBox because it's homogeneous *)
							 												let gboxcontent = makeBoxContent (leafnodelist1 @ [node2]) in
								 											let gbox = ref (Box(op, gboxcontent)) in
								 											let globox = ref (GlobalBox(gbox)) in
							 												let _ = saveBoxNode globox gbox eqclass in
							 												leafnodelist1 @ [node2]
							 											(* if the second son is homogenous then we propagate it's leaves with the other node *)
							 											else if ((not (op = op1)) && (op = op2)) then
							 												(* we could make a partial local GlobalBox because it's homogeneous *)
							 												let gboxcontent = makeBoxContent ([node1] @ leafnodelist2) in
									 										let gbox = ref (Box(op, gboxcontent)) in
									 										let globox = ref (GlobalBox(gbox)) in
								 											let _ = saveBoxNode globox gbox eqclass in
								 											[node1] @ leafnodelist2
							 											else
							 											(* otherwise we only propagate these two nodes *)
							 												[node1 ; node2]
							 									else
							 										[pegnode]
						|	_ -> raise(EPEGError("parcoursPEGHorizontalAbstraction","n-ary operators are not handled yet."))
						end
and
	applyHorizontalAbstraction nodel eqclass = match nodel with
		[] -> []
	|	t::q -> 
			(parcoursPEGHorizontalAbstraction t eqclass)::(applyHorizontalAbstraction q eqclass)


(* Entry point for doing the Horizontal abstraction on a LustrePEG *)
and
	doHorizontalAbstraction peg = match peg with
		LustrePEG(name,eq,eql) -> 
let rootnodes = extractNodeListFromEquivalenceClass eq in 
							 applyHorizontalAbstraction rootnodes eq
;;



(**************************)
(* Vertical Abstraction *)
(**************************)


let rec parcoursPEGVerticalAbstraction_internal pegnode eqclass = match !pegnode with
		PEGLeaf(expr) -> pegnode::[]
	|	BoxNode(op, ab, eq) -> []
	|	GlobalBox(box) -> []	
	|	PEGNode(op,l) -> 
										
						let explicitl = makeExplicitList l in
						begin match explicitl with
						 
						(* meaning it's a UMINUS, a PRE, or a CURRENT *)
						|	[PEGLeaf(_)] -> [pegnode]
						|	[PEGNode(op2,_)] ->  
							 			
							 			let node = (List.nth l 0) in
							 			let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
	 									let nodeclassrelated = getEqClassCorresponding node classbelow in
	 									let leafnodelist = parcoursPEGVerticalAbstraction_internal node nodeclassrelated in
	 									
	 									let _ = 
												if (((List.length leafnodelist) > 2) && (isOperatorSymmetric op2)) then
												let _ = (createVerticalBoxFrom node nodeclassrelated nodeclassrelated leafnodelist op2) in
												()
	 									in
	 									
	 									(* we check if there are other nodes in EqClass and launch the process onto them *)
										(* we don't process twice the root nodes because they are launched by the outer process  *)
		 								let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
	 									let cleanother = removeFromListOnce node othernode in
	 									let _ = applyVerticalAbstraction cleanother nodeclassrelated in
 									
 										[node]
 									
 						(* meaning it's a binary node *)
						|	[PEGLeaf(v1) ; PEGLeaf(v2)] -> 
						 								if (isOperatorSymmetric op) then
							 								let leaf1 = (List.hd l) in
						 									let leaf2 = (List.nth l 1) in
															[leaf1 ; leaf2]
														else
															[pegnode]
														
						|	[PEGLeaf(v1) ; PEGNode(op2,ll)] ->
															let leaf = (List.hd l) in
						 									let node = (List.nth l 1) in
		 													let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in
						 									
						 									let leafnodelist = parcoursPEGVerticalAbstraction_internal node nodeclassrelated in
						 									
						 									(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
							 								let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
						 									let cleanother = removeFromListOnce node othernode in
						 									let _ = applyVerticalAbstraction cleanother nodeclassrelated in
						 									
					 										if (isOperatorSymmetric op) then
							 									(* if we are still in homogeneous area then we propagate all leaves *)
							 									if (op = op2) then
							 										leaf::leafnodelist
							 									(* otherwise we will only propagate these two nodes *)
							 									(* but before we have to make the Vertical boxes if there are enough leaves in the PEGNode *)
							 									else
							 										let _ = 
							 											if (((List.length leafnodelist) > 2) && (isOperatorSymmetric op2)) then
							 											let _ = (createVerticalBoxFrom node nodeclassrelated nodeclassrelated leafnodelist op2) in
							 											()
							 										in
							 										[leaf ; node]
	 														else
	 															let _ = 
						 											if (((List.length leafnodelist) > 2) && (isOperatorSymmetric op2)) then
						 											let _ = (createVerticalBoxFrom node nodeclassrelated nodeclassrelated leafnodelist op2) in
						 											()
						 										in
						 										[pegnode]
																
						|	[PEGNode(op1,ll) ; PEGLeaf(v2)] ->
						 									let node = (List.hd l) in
						 									let leaf = (List.nth l 1) in
						 									let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in
						 									
						 									let leafnodelist = parcoursPEGVerticalAbstraction_internal node nodeclassrelated in
						 									
						 									(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
							 								let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
						 									let cleanother = removeFromListOnce node othernode in
						 									let _ = applyVerticalAbstraction cleanother nodeclassrelated in
						 									
						 									if (isOperatorSymmetric op) then
							 									(* if we are still in homogeneous area then we propagate all leaves *)
							 									if (op = op1) then
							 										leaf::leafnodelist
							 									(* otherwise we will only propagate these two nodes *)
							 									(* but before we have to make the Vertical boxes if there are enough leaves in the PEGNode *)
							 									else
							 										let _ = 
							 											if (((List.length leafnodelist) > 2) && (isOperatorSymmetric op1)) then
							 											let _ = (createVerticalBoxFrom node nodeclassrelated nodeclassrelated leafnodelist op1) in
							 											()
							 										in
							 										[leaf ; node]
	 														else
	 															let _ = 
						 											if (((List.length leafnodelist) > 2) && (isOperatorSymmetric op1)) then
						 											let _ = (createVerticalBoxFrom node nodeclassrelated nodeclassrelated leafnodelist op1) in
						 											()
						 										in
						 										[pegnode]
					
						| 	[PEGNode(op1,l1) ; PEGNode(op2,l2)] -> 
						 										let node1 = (List.hd l) in
						 										let node2 = (List.nth l 1) in
						 										let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 										let nodeclassrelated1 = getEqClassCorresponding node1 classbelow in
						 										let nodeclassrelated2 = getEqClassCorresponding node2 classbelow in
						 									
						 										let leafnodelist1 = parcoursPEGVerticalAbstraction_internal node1 nodeclassrelated1 in
						 										let leafnodelist2 = parcoursPEGVerticalAbstraction_internal node2 nodeclassrelated2 in
						 										
						 										(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode1 = (extractNodeListFromEquivalenceClass nodeclassrelated1) in
							 									let cleanother1 = removeFromListOnce node1 othernode1 in
							 									let _ = applyVerticalAbstraction cleanother1 nodeclassrelated1 in
							 									
							 									(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode2 = (extractNodeListFromEquivalenceClass nodeclassrelated2) in
							 									let cleanother2 = removeFromListOnce node2 othernode2 in
							 									let _ = applyVerticalAbstraction cleanother2 nodeclassrelated2 in
						 									
						 										if (isOperatorSymmetric op) then
						 											(* if we are still in homogeneous area then we propagate all leaves *)
							 										if (op = op1 && op = op2) then
							 											leafnodelist1 @ leafnodelist2
							 										(* otherwise we will only propagate these two nodes *)
							 										(* but before we have to make the Vertical boxes if there are enough leaves *)
							 										else
								 										let _ = 
								 											if (((List.length leafnodelist1) > 2) && (isOperatorSymmetric op1)) then
								 											let _ = (createVerticalBoxFrom node1 nodeclassrelated1 nodeclassrelated1 leafnodelist1 op1) in
								 											()
								 										in
								 										let _ = 
								 											if (((List.length leafnodelist2) > 2) && (isOperatorSymmetric op2)) then
								 											let _ = (createVerticalBoxFrom node2 nodeclassrelated2 nodeclassrelated2 leafnodelist2 op2) in
								 											()
							 											in
							 											[node1 ; node2]
							 									else
							 										let _ = 
								 											if (((List.length leafnodelist1) > 2) && (isOperatorSymmetric op1)) then
								 											let _ = (createVerticalBoxFrom node1 nodeclassrelated1 nodeclassrelated1 leafnodelist1 op1) in
								 											()
								 										in
								 										let _ = 
								 											if (((List.length leafnodelist2) > 2) && (isOperatorSymmetric op2)) then
								 											let _ = (createVerticalBoxFrom node2 nodeclassrelated2 nodeclassrelated2 leafnodelist2 op2) in
								 											()
							 											in
							 											[pegnode]
						 										
						|	_ -> raise(EPEGError("parcoursPEGVerticalAbstraction_internal","n-ary operators are not handled yet."))					
					end
and
		createVerticalBoxFrom node eqclass_current eqclass_top leafl_total operator =
			let rec isNodeInside n l = match l with
					[] -> false
				|	t::q -> if (eqPegNode !t !n) then true else (isNodeInside n q)
			and
			createListWithoutOnce l n = match l with
					[] -> []
				|	t::q -> if (eqPegNode !t !n) then q else t::(createListWithoutOnce q n)
			and createListWithoutAny l forbid = match forbid with
					[] -> []
				|	t::q -> if (isNodeInside t l) then (createListWithoutAny (createListWithoutOnce l t) q) else (createListWithoutAny l q)
			and 
				createVerticalBoxOn l eqc eqt ltot op = match l with
					[] -> []
				|	t::q -> let classbelow = extractEquivalenceClassListFromEquivalenceClass eqc in
							let nodeclassrelated = getEqClassCorresponding t classbelow in
							(createVerticalBoxFrom t nodeclassrelated eqt ltot op) @ (createVerticalBoxOn q eqc eqt ltot op)
			in
			
			let ispresent = isNodeInside node leafl_total in
			if (ispresent) then
				(* we are on a leaf (a value, or a heterogeneous box) *)
				(* we remove this leaf from the list, and make an abstract box with the rest *)
				let leavesleft = createListWithoutOnce leafl_total node in
				let boxcontent = makeBoxContent leavesleft in
				let box = ref (Box(operator, boxcontent)) in
				let boxn = ref (BoxNode(operator, box, eqclass_current)) in
				let _ = saveBoxNode boxn box eqclass_top in
				[node]
			else
				(* we are in an internal node, not a leaf, we remove the leaves below this node *)
				(* and make an abstract box with the rest, then we reccursivly do the rest *)
				
				let nodes = match !node with
						PEGNode(op,l) -> (createVerticalBoxOn l eqclass_current eqclass_top leafl_total operator)
					|	_ -> raise(EPEGError("createVerticalBoxFrom","Only PEGNode should be called recusivly."))
				in
				
				let leavesleft = createListWithoutAny leafl_total nodes in
				if ((List.length leavesleft)>=2) then
					let boxcontent = makeBoxContent leavesleft in
					let box = ref (Box(operator, boxcontent)) in
					let boxn = ref (BoxNode(operator, box, eqclass_current)) in
					let _ = saveBoxNode boxn box eqclass_top in
					nodes
				else
					nodes
and
	parcoursPEGVerticalAbstraction pegnode eqclass = 
		let nodes_out = parcoursPEGVerticalAbstraction_internal pegnode eqclass in 
		if ((List.length nodes_out)>2) then
			match !pegnode with
			|	PEGNode(op,_) -> let _ = (createVerticalBoxFrom pegnode eqclass eqclass nodes_out op) in 
								 nodes_out
			|	_ -> raise(EPEGError("parcoursPEGVerticalAbstraction","Wrong parameter ! Only PEGNode are admited."))
		else 
			nodes_out
and
	applyVerticalAbstraction nodel eqclass = match nodel with
			[] -> []
		|	t::q -> (parcoursPEGVerticalAbstraction t eqclass)::(applyVerticalAbstraction q eqclass)
;;


(* Entry point for doing the Vertical abstraction on a LustrePEG *)
let doVerticalAbstraction peg = match peg with
	LustrePEG(name,eq,eql) -> let rootnodes = extractNodeListFromEquivalenceClass eq in
						 applyVerticalAbstraction rootnodes eq
;;



(*************************)
(* Expansion Abstraction *)
(*************************)

let extractLeafExpressionInSingloton sing = match sing with
	Singloton(node) -> 	begin match !node with
					  	|	PEGLeaf(ex) -> ex
					  	|	_ -> raise(EPEGError("extractLeafExpressionInSingloton","Wrong parameter ! Need a PEGLeaf in a Singloton."))
						end
|	_ -> raise(EPEGError("extractLeafExpressionInSingloton","Wrong parameter ! Need a PEGLeaf in a Singloton."))
;;


let rec saveGlobalBoxOn boxlist eqclass = match boxlist with
		[] -> ()
	|	t::q -> let _ = begin match !t with
						|	GlobalBox(box) -> saveBoxNode t box eqclass
						|	BoxNode(op, ab, eq) -> saveBoxNode t ab eqclass
						|	_ -> raise( EPEGError("saveGlobalBoxOn","Wrong parameter ! Only GlobalBox and BoxNode are admited."))
						end
					in
					saveGlobalBoxOn q eqclass
;;

(* if the box containes some other box with the same operator, it fuses (recursively) all the leaves into one box *)
let rec expandBoxContent contentl extop = match contentl with
		[] -> []
	|	t::q -> begin match !t with
				|	VLeaf(_) -> t :: (expandBoxContent q extop)
				|	VNode(_) -> t :: (expandBoxContent q extop)
				|	Box_(box) -> begin match !box with 
								|	Box(opin, contl) -> if (opin = extop) then
															let internbox = (expandBoxContent contl extop) in
															internbox @ (expandBoxContent q extop)
														else
															t :: (expandBoxContent q extop)
								end
				end
;;

let rec isAGlobalBoxAlreadyPresentIn nodel = match !nodel with
		[] -> false
	|	t::q -> begin match !t with
					|	GlobalBox(_) -> true
					|	_ -> (isAGlobalBoxAlreadyPresentIn (ref q))
				end
;;

let expandBoxNode op ab eq nodel eqclass = match (!ab,!eq) with
		(Box(op_in, cl),Singloton(pleaf)) ->let nodes = (ref (extractNodeListFromEquivalenceClass  eqclass)) in
											if ((op_in = op) && (not (isAGlobalBoxAlreadyPresentIn nodes))) then
												 let newboxcontent = expandBoxContent cl op in
												 let singlotoncontent = extractLeafExpressionInSingloton !eq in
												 let nleaf = ref (VLeaf(singlotoncontent)) in
												 let box = ref (Box(op, newboxcontent@[nleaf])) in
												 let gb = ref (GlobalBox(box)) in
												 [gb]
											 else
												 []
										 
	|	(Box(op_in,cl),EqClass(pl,cbl,bl)) -> let nboxcontent = expandBoxContent cl op_in in
											  if (not((List.length nboxcontent) = (List.length cl))) then
										  			let box = ref (Box(op_in, nboxcontent)) in
										  			let boxn = ref (BoxNode(op, box, eq)) in
										  			[boxn]
										  		else							  		
										  			[]
	|	_ -> raise(EPEGError("expandBoxNode","Wrong parameter ! Need a Box and a valid equivalence class."))
;;

let rec doBoxExpansionAbstractionOn nodel eqclass = match !nodel with
		[] -> []
	|	t::q -> begin match !t with
				|	BoxNode(op, ab, eq) ->  let globox = (expandBoxNode op ab eq nodel eqclass) in 
											let _ = if ((List.length globox)>0) then
														let _ = saveGlobalBoxOn globox eqclass in
														()
													else
														()
											in
											doBoxExpansionAbstractionOn (ref q) eqclass
				| _ -> doBoxExpansionAbstractionOn (ref q) eqclass
				end
;;

let rec applyBoxExpansionAbstractionOn eql = match eql with
		[] -> []
	|	t::q -> begin match !t with
				|	Singloton(_) -> applyBoxExpansionAbstractionOn q
				|	EqClass(pl,_,_) -> let _ = (doBoxExpansionAbstractionOn pl t) in
										(applyBoxExpansionAbstractionOn q)
				|	_ -> raise( EPEGError("applyBoxExpansionAbstraction","Only Singloton and EqClass should be present in the equivalence class list."))
				end
;;


(* Entry point for doing the BoxExpansion abstraction on a LustrePEG *)
let doBoxExpansionAbstraction peg = match peg with
 	LustrePEG(name,eq,eql) -> applyBoxExpansionAbstractionOn !eql
;;



(*******************)
(* Sum Combinaison *)
(*******************)

let rec parcoursSumCombinaisonAbstraction pegnode eqclass = match !pegnode with
		PEGLeaf(expr) -> pegnode::[]
	|	BoxNode(op, ab, eq) -> []
	|	GlobalBox(box) -> []	
	|	PEGNode(op,l) ->
						let explicitl = makeExplicitList l in
						begin match explicitl with
						|	[a] -> [pegnode]
						|	[PEGLeaf(v1) ; PEGLeaf(v2)] -> 	
														let leaf1 = (List.hd l) in
					 									let leaf2 = (List.nth l 1) in
						 								if (op = OP_PLUS) then
						 									[leaf1 ; leaf2]
						 								else
						 									[pegnode]
						 								
						 |	[PEGLeaf(v1) ; PEGNode(op2,ll)] -> 
															let leaf = (List.hd l) in
						 									let node = (List.nth l 1) in
						 									let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in
						 									
						 									let leafnodelist = parcoursSumCombinaisonAbstraction node nodeclassrelated in
						 									(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
						 									let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
						 									let cleanother = removeFromListOnce node othernode in
						 									let _ = applySumCombinaisonAbstractionOn cleanother nodeclassrelated in
						 									
						 									
						 									(* if we are still in homogeneous area of addition then we propagate all leaves *)
						 									if (op = op2) then
						 										if (op = OP_PLUS) then
						 											leaf::leafnodelist
						 										else
						 											[pegnode]
						 									(* otherwise we only propagate the root, but we check if we step out an homogenous zone of addition *)
						 									else
						 										let _ =
						 											if ((op2 = OP_PLUS) && (List.length leafnodelist) > 2) then
						 	
						 												()
						 										in
						 										[pegnode]
						 									
						|	[PEGNode(op1,ll) ; PEGLeaf(v2)] -> 
						 									let node = (List.hd l) in
						 									let leaf = (List.nth l 1) in
						 									let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 									let nodeclassrelated = getEqClassCorresponding node classbelow in
						 									
						 									let leafnodelist = parcoursSumCombinaisonAbstraction node nodeclassrelated in
						 									(* we check if there are other nodes in EqClass and launch the process onto them *)
															(* we don't process twice the root nodes because they are launched by the outer process  *)
						 									let othernode = (extractNodeListFromEquivalenceClass nodeclassrelated) in
						 									let cleanother = removeFromListOnce node othernode in
						 									let _ = applySumCombinaisonAbstractionOn cleanother nodeclassrelated in
						 									
						 									(* if we are still in homogeneous area of addition then we propagate all leaves *)
						 									if (op = op1) then
						 										if (op = OP_PLUS) then
						 											leaf::leafnodelist
						 										else
						 											[pegnode]
						 									(* otherwise we only propagate the root, but we check if we step out an homogenous zone of addition *)
						 									else
						 										let _ =
						 											if ((op1 = OP_PLUS) && (List.length leafnodelist) > 2) then
						
						 												()
						 										in
						 										[pegnode]
						 									
						| 	[PEGNode(op1,l1) ; PEGNode(op2,l2)] -> 
						 										let node1 = (List.hd l) in
						 										let node2 = (List.nth l 1) in
																let classbelow = extractEquivalenceClassListFromEquivalenceClass eqclass in
						 										let nodeclassrelated1 = getEqClassCorresponding node1 classbelow in
						 										let nodeclassrelated2 = getEqClassCorresponding node2 classbelow in
						 									
							 									let leafnodelist1 = parcoursSumCombinaisonAbstraction node1 nodeclassrelated1 in
							 									
							 									(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode1 = (extractNodeListFromEquivalenceClass nodeclassrelated1) in
							 									let cleanother1 = removeFromListOnce node1 othernode1 in
							 									let _ = applySumCombinaisonAbstractionOn cleanother1 nodeclassrelated1 in
							 									
							 									let leafnodelist2 = parcoursSumCombinaisonAbstraction node2 nodeclassrelated2 in
							 									
							 									(* we check if there are other nodes in EqClass and launch the process onto them *)
																(* we don't process twice the root nodes because they are launched by the outer process  *)
							 									let othernode2 = (extractNodeListFromEquivalenceClass nodeclassrelated2) in
							 									let cleanother2 = removeFromListOnce node2 othernode2 in
							 									let _ = applySumCombinaisonAbstractionOn cleanother2 nodeclassrelated2 in
							 									
							 									
						 										(* if we are still in homogeneous area of addition then we propagate all leaves *)
						 										if ((op = op1) && (op = op2)) then
						 											if (op = OP_PLUS) then
						 												leafnodelist1 @ leafnodelist2
						 											else
						 												[pegnode]
						 										else
						 											(* if the first son is homogenous then we propagate it's leaves with the other node *)
						 											if ((op = op1) && (not (op = op2))) then
						 												if (op = OP_PLUS) then
						 													leafnodelist1 @ [node2]
						 												else
						 													if (op2 = OP_PLUS) then
						 													(* we step out the sum contained in the second son, we try to recombinate *)
						 														let _ = 
						 															if ((List.length leafnodelist2) > 2) then
						 																let _ = generateBoxes leafnodelist2 in
						 																()
						 														in
						 														[pegnode]
						 													else
						 														[pegnode]
						 														
						 											(* if the second son is homogenous then we propagate it's leaves with the other node *)
						 											else if ((not (op = op1)) && (op = op2)) then
						 													if (op = OP_PLUS) then
							 													[node1] @ leafnodelist2
							 												else
						 														if (op1 = OP_PLUS) then
							 													(* we step out the sum contained in the first son, we try to recombinate *)
							 														let _ = 
							 															if ((List.length leafnodelist1) > 2) then
							 																let _ = generateBoxes leafnodelist1 in
							 																()
							 														in
							 														[pegnode]
									 											else
									 												[pegnode]
						 														
						 											else
						 											(* none of the operator are homogenous, we check both branches and propagate the root *)
						 												if (op = OP_PLUS) then
						 													[node1;node2]
						 												else
						 													let _ =	
						 														if (op1 = OP_PLUS) then
						 															(* we step out the sum contained in the first son, we try to recombinate *)
						 															if ((List.length leafnodelist1) > 2) then
							 														let _ = generateBoxes leafnodelist1 in
							 														()
							 												in
							 												let _ = 
							 													if (op2 = OP_PLUS) then
						 															(* we step out the sum contained in the second son, we try to recombinate *)
						 															if ((List.length leafnodelist2) > 2) then
							 														let _ = generateBoxes leafnodelist2 in
							 														()
							 												in
						 													[pegnode]

						|	_ -> raise(EPEGError("parcoursSumCombinaisonAbstraction","n-ary operators are not handled yet."))
						end
and
	(* Returns a list of pair [(term,nboccur)...] of redundant elements in the list *)
	extractRedundantTerms pegnodelist wholelist res = 
		let rec nbOccur term list res = match list with
			[] -> res
		|	t::q -> if (eqPegNode !term !t) then (nbOccur term q (res+1)) else (nbOccur term q res)
		in
		match pegnodelist with
			[] -> res
		|	t::q -> let nb = nbOccur t wholelist 0 in
					if (nb > 1) then 
						(extractRedundantTerms q wholelist (res@[(t,nb)])) 
					else 
						(extractRedundantTerms q wholelist res)
and 
	(* order a list of pair [(term,nboccur)...] by increasing order of occurence *)
	orderByFrequence list res = 
		let rec insertCorrectlyIn l elmt resl = match l with
			[] -> res@[elmt]
		|	t::q-> 	begin match [t;elmt] with
					|	[(a,b);(c,d)] -> if (b > d) then
											resl@[elmt;t]@q
										else
											insertCorrectlyIn q elmt (t::resl)
					|	_ -> raise(EPEGError("orderByFrequence#insertCorrectlyIn","Invalid state, element should be pair."))
					end
		in
		match list with
			[] -> res
		|	t::q-> orderByFrequence q (insertCorrectlyIn res t [])
and
	(* return for a given level the maximal factorisation, like f((a,10),4) -> [(a,4);(a,4);(a,2)] *)
	generateOneLevelFactorisation (term,nboccur) level res =
		if (nboccur < level) then
			res@[(term,nboccur)]
		else
			generateOneLevelFactorisation (term, (nboccur-level)) level (res@[(term,level)])
and
	(* return a list of list of terms like : [ [(a,6)] ; [(a,5),(a,1)] ; ... ; [(a,2);(a,2);(a,2)] ] *)
	doDecreasingPartialFactorisation (term,nboccur) total res = match nboccur with
		2 -> let last = generateOneLevelFactorisation (term,total) nboccur [] in
			 [last]@res
	|	_ -> let level = generateOneLevelFactorisation (term,total) nboccur [] in
			 doDecreasingPartialFactorisation (term,(nboccur-1)) total [level]@res
and 
	(* Same as doDreceasingPartialFactorisation but done on a list of term, *)
	(* Returns a list of pair [ (a, [ all_level ]); (b, [ all_level ])...] *)
	doDecreasingPartialFactorisationOn terms = match terms with
		[] -> []
	|	(a,n)::q -> let levels = doDecreasingPartialFactorisation (a,n) n [] in
					(a,levels)::(doDecreasingPartialFactorisationOn q) 
and
	(* main entry point to generate the boxes of sum recombinaison, need only the leaves of the sum *)
	generateBoxes nodelist =
		
					(**********************)
					(* TODO finish this ! *)
					(**********************)
	
		[]
and
	applySumCombinaisonAbstractionOn pl eq = match pl with
		[] -> []
	|	t::q -> let nodes = parcoursSumCombinaisonAbstraction t eq in
				let _ =
					if ((List.length nodes) > 2) then
						let _ = generateBoxes nodes in
						()
				in
				nodes::(applySumCombinaisonAbstractionOn q eq)
;;

(* Entry point for doing the SumCombinaison abstraction on a LustrePEG *)
let doSumCombinaisonAbstraction peg = match peg with
 	LustrePEG(name,eq,eql) -> begin match !eq with
 						 |	EqClass(pl,_,_) -> applySumCombinaisonAbstractionOn !pl eq
 						 |	_ -> []
 						 end
;;



