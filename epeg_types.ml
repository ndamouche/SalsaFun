

open SalsaTypes ;;

exception EPEGError of (string * string);;

type epegCommand = Affectation of string * expression ;;


(****** PEG TYPE ******)

type loperator = OP_PLUS | OP_MINUS | OP_MULT | OP_UMINUS | OP_DIV | OP_SIN | OP_COS | OP_LOG | OP_EXP | OP_SQRT | OP_INTOFBOOL  ;;

type abstractbox = Box of loperator * (boxcontent ref) list		(* meaning : operator name * [arguments] *)

and boxcontent =							(* what a argument could be : *) 
		VLeaf of expression					(* a leaf value, ex: a, b,... or a numeric value *)
	|	VNode of pegnode					(* an un-symmetric node like "-" or "/" *)
	|	Box_ of abstractbox ref					(* or an other box, witch be useful in the box expanding process *)

and pegnode = 	PEGLeaf of expression 
	|	PEGNode of loperator * ((pegnode ref) list)		(* meaning : operator * [arguments] *)
	|	BoxNode of loperator * (abstractbox ref) * (equivalenceclass ref)	(* meaning : operator * a box * an equivalent class *)
	|	GlobalBox of abstractbox ref

and equivalenceclass = 
		None of command
	|	Singloton of (pegnode ref)				(* meaning : a leaf *)
	|	EqClass of ((pegnode ref) list) ref * ((equivalenceclass ref) list) ref * ((abstractbox ref) list) ref		
									(* meaning : [nodes inside] * [eq class below] * [boxes below] *)
;;

(* c'est sur les lustrepeg qu'il faut faire la profitability *)
type lustrepeg = LustrePEG of string * (equivalenceclass ref)  * (((equivalenceclass ref) list) ref)			
		 (* meaning : variable name * root class * all equivalent class nodes *)
;;


(* used to apply the abstraction algorithm : Horizontal *)
let use_distribute_algorithm = ref false ;;
(* used to apply the abstraction algorithm : Vertical *)
let use_factorize_algorithm = ref false ;;






