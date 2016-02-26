
open SalsaTypes ;;
open Epeg_types ;;

(************************************************)
(****** convert the lustre AST into a SPEG ******)
(************************************************)

(* prepare all PEGNode across an expression *)
let rec makePEGNode tree = match tree with 
	Id(n,l)      -> PEGLeaf(tree)
|	Cst(v,l)     -> PEGLeaf(tree)
|	Plus(a,b,_)  -> let pega = ref (makePEGNode a) in
			        let pegb = ref (makePEGNode b) in
				    PEGNode(OP_PLUS, [pega;pegb])
|	Minus(a,b,_) -> let pega = ref (makePEGNode a) in
			        let pegb = ref (makePEGNode b) in
				    PEGNode(OP_MINUS, [pega; pegb])						
|	Times(a,b,_) -> let pega = ref (makePEGNode a) in
			        let pegb = ref (makePEGNode b) in
				    PEGNode(OP_MULT, [pega; pegb])				
|	Div(a,b,_)   -> let pega = ref (makePEGNode a) in
			        let pegb = ref (makePEGNode b) in
				    PEGNode(OP_DIV, [pega; pegb])		
|	Uminus(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_UMINUS, [pegb])
|	Sqrt(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_SQRT, [pegb])
|	Cos(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_COS, [pegb])
|	Sin(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_SIN, [pegb])
|	Exp(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_EXP, [pegb])
|	Log(b,_)  -> let pegb = ref (makePEGNode b) in
				     PEGNode(OP_LOG, [pegb])
|	IntOfBool(be,_)  -> raise (EPEGError ("makePEGNode", "Case note yet implemented: IntOfBool")) 
;;

(* create all equivalent class on operators, and return the root equivalent class *)
let rec makeEquivalentClass pegtree = match !pegtree with
		PEGLeaf(_) -> ref (Singloton(pegtree))
	|	PEGNode(op, args) -> ref (EqClass( ref (pegtree::[]), 
										   ref (makeEquivalentClassOn args) , 
										   (* warning : this is only for typing issue -> this item MUST be destroyed after *)
										   ref ((ref (Box(OP_PLUS, [])))::[])
										 ))
	|	_ -> raise(EPEGError("makeEquivalentClass","Argument should not be a BoxNode at this point of the construction."))
and 
(* call the makeEquivalentClass to make the class, and returns the list of equivalent class related to the given pegnode list *)
	makeEquivalentClassOn ltree = match ltree with
	  [] ->  []
	| t::q -> (makeEquivalentClass t)::(makeEquivalentClassOn q)
;;

(* process one command, if it's an affectation it returns the root equivalent class of the corresponding expression, otherwise it returns a None equivalent class that must be ignored in the futur *)
let processCommand com = match com with
	Affectation(name, tree) -> let pegnod = ref (makePEGNode tree) in
					 (name,makeEquivalentClass pegnod)
;;

(* return for the full list of equivalent class present in the PEG*)
let rec getAllEquivalentClass root = match !root with
		None(_) -> raise(EPEGError("getAllEquivalentClass","None should not be a valid equivalent class in this function"))
	|	Singloton(_) -> root::[]
	|	EqClass(_,cls,_) -> root::(getAllEquivalentClassOn !cls)

and getAllEquivalentClassOn lroot = match lroot with
		[] -> []
	|	t::q -> (getAllEquivalentClass t) @ (getAllEquivalentClassOn q)
;;


(* return a list of speg root corresponding to all affectation command found in the parsed lustre file *)
let convertASTtoSPEG com = 
	let (name,eq) = processCommand com 
        in LustrePEG(name, eq, ref (getAllEquivalentClass eq))
;;


