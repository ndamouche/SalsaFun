

open SalsaTypes ;;
open Num ;;
open Prelude ;;

let spc i = String.make i ' '  ;;


let black =  "\027[30m ";; 
let black_b ="\027[40m ";;
let red ="\027[31m ";;
let red_b ="\027[41m " ;;
let green ="\027[32m ";; 
let green_b="\027[42m ";;
let yellow="\027[33m ";; 
let yellow_b="\027[43m ";;
let blue="\027[34m ";; 
let blue_b="\027[44m ";;
let magenta="\027[35m ";; 
let magenta_b="\027[45m ";;
let cyan="\027[36m ";; 
let cyan_b="\027[46m ";;
let white="\027[37m ";;
let white_b ="\027[47m ";;
let cc = black;;

let rec cleanZeros s = 
  let l = String.length s in
  let c = String.get s (l-1)
  in if c ='0' then cleanZeros (String.sub s 0 (l-1)) else s ;;


let printFInterval x = match x with
  (I(a,b)) -> (* if (*a = b*)false then "["^(cleanZeros (string_of_float b))^"]" 
   
  				else "["^(cleanZeros (string_of_float a))^","^(cleanZeros (string_of_float b))^"]" *)
  			   (cleanZeros (string_of_float ((a+.b)*.0.5)))   
| Empty    -> "Empty" ;;


let printRInterval x = match x with
  (J(a,b)) -> (* if a = b   then "["^(cleanZeros (string_of_float b))^"]" 
   
  				else*)  "["^(cleanZeros (string_of_float a))^","^(cleanZeros (string_of_float b))^"]"
| JInfty   -> "[-oo,+oo]"
| JEmpty    -> "Empty" ;;

(*
let printCst (u,v) = "{"^blue^(printFInterval u)^cc^";"^magenta^(printRInterval v)^cc^"}" ;;
*)
let printCst (u,v) = (printFInterval u) ;;

let rec printPhis phis = match phis with
  [] -> ""
| (Phi(id,id1,id2))::phis' -> (id^" = "^id1^" U "^id2^" "^(printPhis phis')) ;;


let rec printExpression e = match e with
  Cst(x,_)              -> printCst x
| Plus(e1,e2,_)         -> ("(" ^ (printExpression e1) ^ " + " ^ (printExpression e2) ^ ")" ) 
| Times(e1,e2,_)        -> ("(" ^ (printExpression e1) ^ " * " ^ (printExpression e2) ^ ")" )
| Minus(e1,e2,_)        -> ("(" ^ (printExpression e1) ^ " - " ^ (printExpression e2) ^ ")" )
| Div(e1,e2,_)          -> ("(" ^ (printExpression e1) ^ " / " ^ (printExpression e2) ^ ")" )
| Uminus(e1,_)	        -> ("( " ^ "-" ^ (printExpression e1) ^ ")" )
| Sqrt(e1,_)	        -> ("sqrt(" ^ (printExpression e1) ^ ")" )
| Cos(e1,_)	        -> ("cos(" ^ (printExpression e1) ^ ")" )
| Sin(e1,_)	        -> ("sin(" ^ (printExpression e1) ^ ")" )
| Exp(e1,_)	        -> ("exp(" ^ (printExpression e1) ^ ")" )
| Log(e1,_)	        -> ("log(" ^ (printExpression e1) ^ ")" )
| IntOfBool(be,_)       -> ("intOfBool(" ^ (printBoolExpr be) ^ ")" )
| Id(x,_)	        -> x 


 and  printBoolExpr b = match b with
  BCst(b,_)  		-> if b then "true" else "false" 
| Eq(e1,e2,_)   	-> ("(" ^ (printExpression e1) ^ " == " ^ (printExpression e2) ^ ")") 
| Lt(e1,e2,_)   	-> ("(" ^ ( printExpression e1) ^ " < " ^ (printExpression e2) ^ ")") 
| Lte(e1,e2,_)   	-> ("(" ^ ( printExpression e1) ^ " <= " ^ (printExpression e2) ^ ")") 
| Gt(e1,e2,_)   	-> ("(" ^ ( printExpression e1) ^ " > " ^ (printExpression e2) ^ ")") 
| Gte(e1,e2,_)   	-> ("(" ^ ( printExpression e1) ^ " >= " ^ (printExpression e2) ^ ")") 
| And(e1,e2,_)   	-> ("(" ^ ( printBoolExpr e1) ^ " && " ^ (printBoolExpr e2) ^ ")") 
| Or(e1,e2,_)   	-> ("(" ^ ( printBoolExpr e1) ^ " || " ^ (printBoolExpr e2) ^ ")") 
| Not(e1,_)       	-> ("(!" ^ ( printBoolExpr e1) ^ ")") 
| BoolOfInt(e1,_) 	-> ("(BoolOfInt(" ^ (printExpression e1) ^ ")") 
;;

let rec printCommand c i = match c with
  Assign(id,e,_)	     -> ((spc i)^ id ^ " = " ^  (printExpression e) ^" ;" )
| While(e,c,phis,_)   	     -> ((spc i)^("while " ^ (printBoolExpr e) ^ " {\n"^(printCommand c (i+2))^"\n"^(spc i)^"} ;"  ^ (printPhis phis) ))
| Seq(c1,c2,l) 		     -> ((printCommand c1 i) ^ "\n"  ^ (printCommand c2 i))
| Cond(b,c1,Nop(_),phis,_)   -> ((spc i)^"if " ^ (printBoolExpr  b) ^ " { \n" ^ (printCommand c1 (i+2)) ^"\n"^(spc i)^ "} ;" ^ (printPhis phis)  )
| Cond(b,c1,c2,phis,_)       -> ((spc i)^"if " ^ (printBoolExpr  b) ^ " { \n" ^ (printCommand c1 (i+2)) ^"\n"^(spc i)^ "} else\n"^(spc i)^"{\n"^ (printCommand c2 (i+2)) ^ "\n"^(spc i)^ "} ;"  ^ (printPhis phis) )
| Nop(_)                     -> (spc i)^"nop ;"
;;


let rec printEnv env = match env with
  [] -> ""
| (id,v)::env' -> ("\n"^(removeSSA id)^" = "^(printCst v))^(printEnv env') ;;


let filePrintFInterval x = match x with
  (I(a,b)) -> if a = b then (cleanZeros (string_of_float b)) 
  			  else "["^(cleanZeros (string_of_float a))^","^(cleanZeros (string_of_float b))^"]"
| Empty    -> "Empty" ;;


let filePrintFIntervalErr x = match x with
  (I(a,b)) -> (cleanZeros (string_of_float a))^","^(cleanZeros (string_of_float b))
| Empty    -> "Empty" ;;


let filePrintEIntervalErr x = match x with
  (J(a,b)) -> (cleanZeros (string_of_float a))^","^(cleanZeros (string_of_float b))
| JEmpty    -> "Empty" 
| JInfty -> "-oo,+oo" ;;



let filePrintCstErr (u,v) = ("["^(filePrintFIntervalErr u)^","^(filePrintEIntervalErr v)^"]") ;;
let filePrintCst (u,v) b =  if (not b) then (filePrintFInterval u) else filePrintCstErr (u,v) ;; 


let rec filePrintFunCallArgs args b = match args with 
[]  -> "" 
| a::[] -> (filePrintExpression (removeSSAExpr a) b) 
| a::aa -> (filePrintExpression (removeSSAExpr a) b)  ^ "," ^ (filePrintFunCallArgs aa b)


and filePrintExpression e b = match e with
  Cst(x,_)              -> filePrintCst x b
| Plus(e1,e2,_)         -> ("(" ^ (filePrintExpression e1 b) ^ " + " ^ (filePrintExpression e2 b) ^ ")" ) 
| Times(e1,e2,_)        -> ("(" ^ (filePrintExpression e1 b) ^ " * " ^ (filePrintExpression e2 b) ^ ")" )
| Minus(e1,e2,_)        -> ("(" ^ (filePrintExpression e1 b) ^ " - " ^ (filePrintExpression e2 b) ^ ")" )
| Div(e1,e2,_)          -> ("(" ^ (filePrintExpression e1 b) ^ " / " ^ (filePrintExpression e2 b) ^ ")" )
| Uminus(e1,_)	        -> ("-" ^ (filePrintExpression e1 b) )
| Sqrt(e1,_)	        -> ("sqrt(" ^ (filePrintExpression e1 b) ^ ")" )
| Cos(e1,_)	        -> ("cos(" ^ (filePrintExpression e1 b) ^ ")" )
| Sin(e1,_)	        -> ("sin(" ^ (filePrintExpression e1 b) ^ ")" )
| Exp(e1,_)	        -> ("exp(" ^ (filePrintExpression e1 b) ^ ")" )
| Log(e1,_)	        -> ("log(" ^ (filePrintExpression e1 b) ^ ")" )
| IntOfBool(be,_)       -> ("intOfBool(" ^ (filePrintBoolExpr be b) ^ ")" )
| Id(x,_)	        -> x
| FunCall(name, args, _)-> (name ^ "(" ^ (filePrintFunCallArgs args b) ^")"  )


and filePrintBoolExpr b b' = match b with
  BCst(b,_)  		-> if b then "true" else "false" 
| Eq(e1,e2,_)   	-> ("(" ^ (filePrintExpression e1 b') ^ " == " ^ (filePrintExpression e2 b') ^ ")") 
| Lt(e1,e2,_)   	-> ("(" ^ ( filePrintExpression e1 b') ^ " < " ^ (filePrintExpression e2 b') ^ ")") 
| Lte(e1,e2,_)   	-> ("(" ^ ( filePrintExpression e1 b') ^ " <= " ^ (filePrintExpression e2 b') ^ ")") 
| Gt(e1,e2,_)   	-> ("(" ^ ( filePrintExpression e1 b') ^ " > " ^ (filePrintExpression e2 b') ^ ")") 
| Gte(e1,e2,_)   	-> ("(" ^ ( filePrintExpression e1 b') ^ " >= " ^ (filePrintExpression e2 b') ^ ")")
| And(e1,e2,_)   	-> ("(" ^ ( filePrintBoolExpr e1 b') ^ " && " ^ (filePrintBoolExpr e2 b') ^ ")") 
| Or(e1,e2,_)   	-> ("(" ^ ( filePrintBoolExpr e1 b') ^ " || " ^ (filePrintBoolExpr e2 b') ^ ")") 
| Not(e1,_)     	-> ("( ! " ^ ( filePrintBoolExpr e1 b') ^ ")")  
| BoolOfInt(e1,_) 	-> ("(BoolOfInt(" ^ (filePrintExpression e1 b') ^ ")") 
;;



let rec memberStringSSA s l = match l with
  [] -> false
| (x,_)::xs -> if (((String.compare (removeSSA x) s)=0) ) 
	           then true else memberStringSSA s xs ;;


let rec filePrintCommandAux c i q = match c with
  Assign(id,Cst(c,l),_)	-> if (memberStringSSA (removeSSA id) !globalEnv) then "nop ;" 
						   else ((spc i)^ id ^ " = " ^  (filePrintExpression (Cst(c,l)) q)^" ;")
| Assign(id,e,_)	-> ((spc i)^ id ^ " = " ^  (filePrintExpression e q)^" ;")
| While(e,c,phis,_)   	-> ((spc i)^("while " ^ (filePrintBoolExpr e q) ^ " {\n"^(filePrintCommandAux c (i+2) q)^"\n"^(spc i)^"} ;")) (*^(printPhis phis)*)
| Seq(c1,c2,l) 		-> ((filePrintCommandAux c1 i q) ^ "\n" ^ (filePrintCommandAux c2 i q)) 
| Cond(b,c1,Nop(_),phis,_)   -> ((spc i)^"if " ^ (filePrintBoolExpr  b q) ^ " { \n" ^ (filePrintCommandAux c1 (i+2) q) ^"\n"^(spc i)^ "} ;"  )
| Cond(b,c1,c2,phis,_)       -> ((spc i)^"if " ^ (filePrintBoolExpr  b q) ^ " { \n" ^ (filePrintCommandAux c1 (i+2) q) ^"\n"^(spc i)^ "} else\n"^(spc i)^"{\n"^ (filePrintCommandAux c2 (i+2) q) ^ "\n"^(spc i)^ "} ;" )
| Nop(_)                -> (spc i)^"nop ;"
;;


let rec filePrintEnv env = match env with
  [] -> ""
| (id,v)::env' -> ("\n"^(removeSSA id)^" = "^(filePrintCstErr v))^(filePrintEnv env') ;;


let filePrintCommand c varRef initEnv q = 
(*  let a = (filePrintEnv initEnv) in
  let b = ("\n%salsa%\n") in
  *)
  let c = filePrintCommandAux c 2 q in 
		(c) ;;


let rec filePrintArgs args = match args with 
  []    -> "" 
| a::[] -> "double "^ (removeSSA a) 
| a::aa -> "double "^ (removeSSA a) ^ "," ^ (filePrintArgs aa)
;; 


let filePrintFunc f = match f with 
  Func(name, args, c, return, lab)  -> "double "^ name ^ "(" ^ (filePrintArgs args) ^ ") {\n" ^ (filePrintCommand (removeSSACmd c) return [] true) ^ "\n  return " ^ (removeSSA return) ^ " ;\n} \n \n"  
;; 


let rec filePrintFuncList fl = match fl with 
  []    -> ""
| f::fs -> (filePrintFunc f) ^ (filePrintFuncList fs) 
;; 







