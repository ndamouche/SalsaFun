
open Types ;;
open Num ;;
open Prelude ;;
open Ssa ;;
open Print ;;

let printFInterval x = match x with
  (I(a,b)) -> if a = b || a = -. b  then "["^(string_of_float b)^"]" 
   
  				else "["^(string_of_float a)^","^(string_of_float b)^"]"
| Empty    -> "Empty" ;;


let printRInterval x = match x with
  (J(a,b)) -> if a =/ b || (a -/ b =/ (num_of_int 0))  then "["^(approx_num_exp 15 b)^"]" 
   
  				else "["^(approx_num_exp 15 a)^","^(approx_num_exp 15 b)^"]"
| JInfty   -> "[-oo,+oo]"
| JEmpty    -> "Empty" ;;


let printCst (u,v) = "{"^(printFInterval u)^(printRInterval v)^"}";;


let fmin4 a b c d = min (min a b) (min c d) ;;

let fmax4 a b c d = max (max a b) (max c d) ;;

let rmin a b = min_num a b ;;

let rmax a b = max_num a b ;;

let rmin4 a b c d = rmin (rmin a b) (rmin c d) ;;

let rmax4 a b c d = rmax (rmax a b) (rmax c d) ;;


(**********************************************************************************)
(*********************************** Intervals ************************************)
(**********************************************************************************)


let fPlus  x y = match (x,y) with
  ((I(v0Min,v0Max)),(I(v1Min,v1Max))) -> let a = v0Min +. v1Min in
                         		 let b = v0Max +. v1Max in I(a,b)
| _ -> Empty
;;
   

let fMinus  x y = match (x,y) with
  ((I(v0Min,v0Max)),(I(v1Min,v1Max))) -> let a = v0Min -. v1Max in
   			                 let b = v0Max -. v1Min in I(a,b)
| _ -> Empty
;;

let fMult x y = match (x,y) with
  ((I(v0Min,v0Max)),(I(v1Min,v1Max))) -> let f1 = (v0Min *. v1Min) in
                                         let f2 = (v0Min *. v1Max) in
                                         let f3 = (v0Max *. v1Min) in
                                         let f4 = (v0Max *. v1Max) in I((fmin4 f1 f2 f3 f4,fmax4 f1 f2 f3 f4))
| _ -> Empty
;;

let fDiv x y = match (x,y) with 
    ((I(v0Min,v0Max)),(I(v1Min,v1Max))) -> let f1 = (v0Min /. v1Min) in
                                           let f2 = (v0Min /. v1Max) in
                                           let f3 = (v0Max /. v1Min) in
                                           let f4 = (v0Max /. v1Max) in I((fmin4 f1 f2 f3 f4,fmax4 f1 f2 f3 f4))
| _ -> Empty
;;


let fSqrt x = match x with 
    (I(v0Min,v0Max)) -> I(sqrt v0Min,sqrt v0Max)
| _ -> Empty
;;



let fMeet x y = match (x,y) with
  (I(a,b),I(c,d)) -> 
  		let (u,v) = (max a c,min b d) in
    		if (u>v) then Empty else I(u,v) 
| (Empty,_) -> y
| (_,Empty) -> x 
;;

    
let fJoin x y = match (x,y) with
  ((I(a,b)),(I(c,d))) -> (I(min a c,max b d)) 
| (_,Empty) -> x
| (Empty,_) -> y ;;


let fSseq i1 i2 = match i1 with
  Empty  -> true
| I(a,b) -> (match i2 with
	       Empty  -> false
	     | I(c,d) -> (a>=c) && (b<=d)
	    ) 
;;


let fWidenLeft x y = match (x,y) with
  ((I(a,a')),(I(b,b'))) -> if (a <= b) then a 
                           else 
                              if (b >= 0.0) then 0.0 else neg_infinity
| (Empty,(I(b,b'))) -> b 
| ((I(a,a')),Empty) -> a
| _ -> raise (Error "cannot widenLeft Empty and Empty :-(")
;; 


let fWidenRight  x y = match (x,y) with
  ((I(a,a')),(I(b,b'))) -> if (a' >= b') then a' 
                           else 
					         if (b' <= 0.0) then 0.0 else infinity
| (Empty,(I(b,b'))) -> b' 
| ((I(a,a')),Empty) -> a'
| _ -> raise (Error "cannot widenRight Empty and Empty :-(")
;; 


let fWiden i1 i2 = (I(fWidenLeft i1 i2 , fWidenRight i1 i2)) ;; 

(**********************************************************************************)
(********************************** JIntervals ************************************)
(**********************************************************************************)


let rPlus  x y = match (x,y) with
  ((J(v0Min,v0Max)),(J(v1Min,v1Max))) -> let a = v0Min +/ v1Min in
                         		 let b = v0Max +/ v1Max in J(a,b)
| (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
| _ -> JEmpty
;;
   

let rMinus  x y = match (x,y) with
  ((J(v0Min,v0Max)),(J(v1Min,v1Max))) -> let a = v0Min -/ v1Max in
   			                 let b = v0Max -/ v1Min in J(a,b)
| (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
| _ -> JEmpty
;;

let rMult x y = match (x,y) with
  ((J(v0Min,v0Max)),(J(v1Min,v1Max))) -> let f1 = (v0Min */ v1Min) in
                                         let f2 = (v0Min */ v1Max) in
                                         let f3 = (v0Max */ v1Min) in
                                         let f4 = (v0Max */ v1Max) in J((rmin4 f1 f2 f3 f4,rmax4 f1 f2 f3 f4))
| (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
| _ -> JEmpty
;;

let rDiv x y = match (x,y) with 
  ((J(v0Min,v0Max)),(J(v1Min,v1Max))) -> let f1 = (v0Min // v1Min) in
                                         let f2 = (v0Min // v1Max) in
                                         let f3 = (v0Max // v1Min) in
                                         let f4 = (v0Max // v1Max) in J((rmin4 f1 f2 f3 f4,rmax4 f1 f2 f3 f4))
| (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
| _ -> JEmpty
;;


let rSqrt x = match x with 
  (J(v0Min,v0Max)) -> J(v0Min **/ (r_of_f_aux 0.5),v0Max **/ (r_of_f_aux 0.5))
| JInfty -> JInfty
| _ -> JEmpty
;;


let rMeet x y = match (x,y) with
  (J(a,b),J(c,d)) -> 
  		let (u,v) = (rmax a c,rmin b d) in
    		if (u >/ v) then JEmpty else J(u,v) 
| (_,JInfty) -> x
| (JInfty,_) -> y
| (JEmpty,_) -> JEmpty
| (_,JEmpty) -> JEmpty
;;

    
let rJoin x y = match (x,y) with
  ((J(a,b)),(J(c,d))) -> (J(rmin a c,rmax b d)) 
| (_,JEmpty) -> x
| (JEmpty,_) -> y 
| (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
;;


let rSseq i1 i2 = match i1 with
  JEmpty  -> true
| J(a,b) -> (match i2 with
	       JEmpty  -> false
             | JInfty -> true
	     | J(c,d) -> (a >=/ c) && (b <=/ d)
	    ) 
| JInfty -> (match i2 with
               JInfty -> true
	     | _ -> false
	    )
;;


let rWidenLeft x y = match (x,y) with
  ((J(a,a')),(J(b,b'))) -> if (a <=/ b) then a 
                           else 
                              if (b >=/ (num_of_int 0)) then (num_of_int 0) else (num_of_int (int_of_float neg_infinity))
| (JEmpty,(J(b,b'))) -> b 
| ((J(a,a')),JEmpty) -> a
| _ -> raise (Error "cannot rwidenLeft Empty and Empty :-(")
;; 


let rWidenRight  x y = match (x,y) with
  ((J(a,a')),(J(b,b'))) -> if (a' >=/ b') then a' 
                           else 
                              if (b' <=/ (num_of_int 0)) then (num_of_int 0) else (num_of_int (int_of_float infinity))
| (JEmpty,(J(b,b'))) -> b' 
| ((J(a,a')),JEmpty) -> a'
| _ -> raise (Error "cannot rwidenRight Empty and Empty :-(")
;; 


let rWiden i1 i2 = match (i1,i2) with
  (_,JInfty) -> JInfty
| (JInfty,_) -> JInfty
| _ -> (J(rWidenLeft i1 i2 , rWidenRight i1 i2)) ;; 



(*
let ulpAux x y = let (_,expx) = frexp x in
	         let (_,expy) = frexp y in
                 (* 57 = 53 + 1 (for halfulp) + 3 (because of frexp) *)
                 let e = (max expx expy) - 57 in
                 let u = (num_of_int 2) **/ (num_of_int e) in 
                   J((num_of_int 0)-/ u,u) ;;
*)
let ulpAux x y = 
	             let z = r_of_f_aux (max (abs_float x) (abs_float y)) in
                 let u = z */ ((num_of_int 2) **/ (num_of_int (-53))) in 
                   J((num_of_int 0)-/ u,u) ;;



let ulp z = match z with 
  (I(x,y)) ->  if ((x = neg_infinity) || (y = infinity) || (y = neg_infinity) || (x = infinity)) then 
                 JInfty  
               else
                 if (x=y) then
                   let (r,_) = modf x in
                     if (r=0.0) then J(num_of_int 0,num_of_int 0)
                     else 
                       ulpAux x y
                 else
                   ulpAux x y
| _ -> JEmpty ;;


(**********************************************************************************)
(******************************** Floats + Errors *********************************)
(**********************************************************************************)


let fePlus (f1,e1) (f2,e2) = 
  let f = fPlus f1 f2 in
  let u = ulp f in
  let e = rPlus (rPlus e1 e2) u 
  in (f,e) ;;



let feMinus (f1,e1) (f2,e2) =
  let f = fMinus f1 f2 in
  let u = ulp f in
  let e = rPlus (rMinus e1 e2) u
  in (f,e) ;;
  

let feUMinus (f1,e1) = feMinus (I(0.0,0.0),J(num_of_int 0,num_of_int 0)) (f1,e1) ;;


let feMult (f1,e1) (f2,e2) =
  let f = fMult f1 f2 in
  let u = ulp f in
  let e = rPlus (rPlus (rMult (r_of_f f1) e2) 
                       (rMult (r_of_f f2) e1)
                ) 
                (rPlus (rMult e1 e2) u) 
  in (f,e) ;;
  
   
let feDiv (f1,e1) (f2,e2) =
    let f = fDiv f1 f2 in
    let u = ulp f in 
 let e = rPlus (rPlus u (rDiv e1 (r_of_f f2))) 
	  		   (rDiv (rMult e1 e2) 
		    		 (rMult (r_of_f f2) (rPlus (r_of_f f2) e2)) 
      		   )
    in (f,e) ;; 
  

let feSqrt (f1,e1) = 	let f = fSqrt f1 in
						let ef1 = rDiv e1 (r_of_f f1) in
						let ef2 = rMult ef1 ef1 in
						let ef3 = rMult ef1 ef2 in
						let ef4 = rMult ef2 ef2 in
						let o1 = rDiv ef1 (J(num_of_int 2,num_of_int 2)) in
						let o2 = rDiv ef2 (J(num_of_int 8,num_of_int 8)) in
						let o3 = rDiv ef3 (J(num_of_int 16,num_of_int 16)) in
						let o4 = rDiv ef4 (J(num_of_int 128,num_of_int 128)) in
						let e' = rPlus (rMinus o1 o2) (rMinus o3 o4)   in
						let e'' = rMult e' (r_of_f f) in
						let e''' = rPlus e'' (ulp f)  
							in (f,e''') 
;;

 
let feJoin (a,b) (c,d)  = 
		(fJoin a c,rJoin b d) ;;
  
  
let feSseq (f,e) (f',e') = (fSseq f f') && (rSseq e e') ;;  


let feLt (f,e) (f',e') = match (f,f') with (* 1 = true, -1 = false, 0 = ? *)
  (I(a,b),I(c,d)) -> if (b < c) then 1 else 
                     if (d < a) then (-1) else 0 
| _ -> 0 ;;


let feGt (f,e) (f',e') = match (f,f') with (* 1 = true, -1 = false, 0 = ? *)
  (I(a,b),I(c,d)) -> if (d < a) then 1 else 
                     if (b < c) then (-1) else 0  
| _ -> 0 ;;


let feEq (f,e) (f',e') = match (f,f') with 
  (I(a,b),I(c,d)) -> if ((a=b) && (b=c) && (c=d)) then 1
		     else
  			if ((b < c) || (d < a)) then (-1) else 0
| (Empty,Empty) -> 1
| _ -> -1 ;;


let errLt (f,e) (f',e') = match (e,e') with
  (J(a,b),J(c,d)) -> if ((rmax (abs_num a) (abs_num b)) </ (rmax (abs_num c) (abs_num d))) then (-1) else (1) 
| (J(_),JEmpty) -> 0
| (JEmpty,J(_)) -> 0
| (JEmpty,JEmpty) -> 0 
| (JInfty,JInfty) -> 0
| (_,JInfty) -> 1
| (JInfty,_) -> (-1)
;;


let feWiden (f,e) (f',e') = ((fWiden f f'),(rWiden e e'));;


(**********************************************************************************)
(****************************** Eval of Expressions *******************************)
(**********************************************************************************)

let rec execWPhis phis env = match phis with
  [] -> env
| (Phi(id,id1,id2))::phis' ->
		let v1 = try (getEnv id1 env) with _ -> (Empty,JEmpty) in  
		let v2 = try (getEnv id2 env) with _ -> (Empty,JEmpty) in  
		let v = feJoin v1 v2
		in execWPhis phis' (setEnv id v env) ;;


let rec execExpr e env = match e with
  Cst(c,_) -> c
| Id(x,_) -> getEnv x env
| Plus(e1,e2,_) -> let v1 = execExpr e1 env in
                   let v2 = execExpr e2 env in fePlus v1 v2
| Minus(e1,e2,_) -> let v1 = execExpr e1 env in
                    let v2 = execExpr e2 env in feMinus v1 v2
| Times(e1,e2,_) -> let v1 = execExpr e1 env in
                    let v2 = execExpr e2 env in feMult v1 v2
| Div(e1,e2,_) -> let v1 = execExpr e1 env in
                  let v2 = execExpr e2 env in feDiv v1 v2
| Uminus(e1,_) -> let v1 = execExpr e1 env in feMinus (fZero,rZero) v1
| Sqrt(e1,_) -> let v1 = execExpr e1 env in feSqrt v1
;;


let execTest be env = match be with
  BCst(b,_) -> if b then 1 else -1 
| Lt(e1,e2,_) -> let v1 = execExpr e1 env in
                 let v2 = execExpr e2 env in feLt v1 v2
| Gt(e1,e2,_) -> let v1 = execExpr e1 env in
                 let v2 = execExpr e2 env in feGt v1 v2
| Eq(e1,e2,_) -> let v1 = execExpr e1 env in
                 let v2 = execExpr e2 env in feEq v1 v2 ;;


let rec envSseq env1 env2 = match env1 with
  [] -> true
| (id,v)::env1' -> try 
			let v' = getEnv id env2
	        	in if (feSseq v v') then (envSseq env1' env2) else false  
		   with
	        	GetEnvError(_) -> false
;;


let rec envWiden env1 env2 = match env1 with
  [] -> env2  
| (id,v)::env1' -> try  
		        let v' = getEnv id env2 in 
  			let vRes = feWiden v v' in 
                        let env3 = setEnv id vRes env2 in
  	        		envWiden env1' env3 
 		   with
 		      GetEnvError(_) -> let env3 = setEnv id v env2 in
                       envWiden env1' env3  
;;


let rec envJoin env1 env2 = match env1 with
  [] -> env2
| (id,v)::env1' -> try 
			let v' = getEnv id env2
		        in envJoin env1' (setEnv id (feJoin v v') env2) 
		   with
		     _ -> envJoin env1' (setEnv id v env2) 
;;
				     
			
let rec envPhiAux phis env1 env2 env = match phis with
  [] -> env
| Phi(id,id1,id2)::phis' -> let v1 = try getEnv id1 env1 with _ -> (Empty,JEmpty) in
                            let v2 = try getEnv id2 env2 with _ -> (Empty,JEmpty) in
                            let env' = setEnv id (feJoin v1 v2) env in 
                                envPhiAux phis' env1 env2 env' ;;


let envPhi phis env1 env2 = let env' = envJoin env1 env2 in envPhiAux phis env1 env2 env' ;;


let rec memberPhis id phis = match phis with
  [] -> false
| Phi(_,id1,id2)::phis' -> if (((String.compare id id1)=0) || ((String.compare id id2)=0)) then true else memberPhis id phis' ;;


let rec evalExprExactAux e env = match e with
  Cst((i,j),_)      -> (r_of_f i,j)    
| Id(x,_)    		-> (try let v = (getEnv x env) in (r_of_f (fst v),snd v) with _ -> (JEmpty,JEmpty))
| Plus(e1,e2,_)   	-> let (i1,j1) = evalExprExactAux e1 env in
                 	   let (i2,j2) = evalExprExactAux e2 env in (rPlus i1 i2,rPlus j1 j2)
| Minus(e1,e2,_)     	-> let (i1,j1) = evalExprExactAux e1 env in
                    	   let (i2,j2) = evalExprExactAux e2 env in (rMinus i1 i2,rMinus j1 j2)
| Times(e1,e2,_)  	-> let (i1,j1) = evalExprExactAux e1 env in
                 	   let (i2,j2) = evalExprExactAux e2 env in (rMult i1 i2,rPlus (rPlus (rMult i1 j2) (rMult j1 i2)) (rMult j1 j2))   
| Div(e1,e2,_)  	-> let (i1,j1) = evalExprExactAux e1 env in
                 	   let (i2,j2) = evalExprExactAux e2 env in 
                       let e = rPlus (rDiv j1 i2) 
	  		                         (rDiv (rMult j1 j2) 
		    		                       (rMult i2 (rPlus i2 j2)) 
      		                         )
                       in (rDiv i1 i2,e)
| Uminus(e1,_) 		-> let (i,j) = evalExprExactAux e1 env in (rMinus rZero i,rMinus rZero j) 
| Sqrt(e1,_) -> let (i,j) = evalExprExactAux e1 env in (rSqrt i,rSqrt j)
;;


let evalExprExact e env =
  let (f,e) = evalExprExactAux e env in
  let f' = f_of_r f in
    (f',rPlus (ulp f') e) ;;

     
let rec evalExpr e env = match e with
  Cst(c,_)      		    -> c 
  
| Id(x,_)    				-> (try (getEnv x env) with _ -> (Empty,JEmpty)) 

| Plus(e1,e2,_)   			-> let v1 = evalExpr e1 env in
                  			   let v2 = evalExpr e2 env in fePlus v1 v2

| Minus(e1,e2,_)            -> let v1 = evalExpr e1 env in 
					           let v2 = evalExpr e2 env in feMinus v1 v2 
							
| Times(e1,e2,_)  			-> let v1 = evalExpr e1 env in 
 	                           let v2 = evalExpr e2 env in feMult v1 v2    
 
| Div(e1,e2,_) 				-> let v1 = evalExpr e1 env in
 					           let v2 = evalExpr e2 env in feDiv v1 v2 
 							                    
| Uminus(e1,_) 				-> let v1 = evalExpr e1 env in feMinus (fZero,rZero) v1 
| Sqrt(e1,_) 				-> let v1 = evalExpr e1 env in feSqrt v1  ;;


let rec evalBoolExpression b env = match b with  
  BCst(b,_)			-> if b then (env,[]) else ([],env)
| Eq(Id(x,_),Cst(v,_),_) 	-> let (fx,ex) = getEnv x env in
				   let (c,d) = getI v in
 				   let resTrue = fMeet fx (I(c,d)) in
				   let resFalse = fJoin (fMeet fx (I(neg_infinity,c))) (fMeet fx (I(d,infinity))) in
				   let envTrue = setEnv x (resTrue,ex) env in
				   let envFalse = setEnv x (resFalse,ex) env in
				     (envTrue,envFalse)		   
| Lt(Id(x,_),Cst(v,_),_)	-> let (fx,ex) = getEnv x env in
				   let (c,d) = getI v in
				   let resTrue = fMeet fx (I(neg_infinity,d)) in
				   let resFalse = fMeet fx (I(c,infinity)) in 
				   let envTrue = setEnv x (resTrue,ex) env in
				   let envFalse = setEnv x (resFalse,ex) env in
				     (envTrue,envFalse)
| Lt(Cst(v,_),Id(x,_),_)	-> let (fx,ex) = getEnv x env in
				   let (c,d) = getI v in
				   let resTrue = fMeet fx (I(c,infinity)) in
				   let resFalse = fMeet fx (I(neg_infinity,d)) in
				   let envTrue = setEnv x (resTrue,ex) env in
				   let envFalse = setEnv x (resFalse,ex) env in
				     (envTrue,envFalse)	
| Gt(e1,e2,l) -> evalBoolExpression (Lt(e2,e1,l)) env 

							     						     
| _ -> raise (Error ("evalBoolExpression: case not yet implemented :-((("))
;;

let simplifyV v = match v with
  (I(a,b),J(c,d)) -> (I(a,b),J(floor_num c,ceiling_num d))
| _ -> v ;;


let rec evalCommand c env = match c with 
  Nop(_)		-> env 
| Assign(id,e,_)	-> let v = evalExpr e env in setEnv id (simplifyV v) env	
| Cond(b,c1,c2,phis,_) 	-> let v = try execTest b env with _ -> 0 in 
			   let (envTrue,envFalse) = evalBoolExpression b env (*getAssignedVarsExact c*) 
                           in (if (v>0) then 
				   let envTrue' = evalCommand c1 envTrue in (envPhi phis envTrue' []) 
                                else (if (v<0) then  
                           		   let envFalse' = evalCommand c2 envFalse in (envPhi phis [] envFalse') 
				      else 
					   let envTrue'  = evalCommand c1 envTrue in 
					   let envFalse' = evalCommand c2 envFalse in (envPhi phis envTrue' envFalse')	
				     )
                              )	
| Seq(c1,c2,_)		-> let env1 = evalCommand c1 env in
        	   	        evalCommand c2 env1 
| While(b,c,phis,l)	-> let env' = List.fold_left (fun env (Phi(x,y,z)) -> (setEnv x (getEnvComplete z env (getEnvComplete y env (Empty,JEmpty))) env)) env phis (* et ici faire join*)in
                         let (envTrue,envFalse) = evalBoolExpression b env' 
				         in exactFixPoint b c envTrue phis (*faire fixpoint pour analyse stat*)

(*
and fixPoint b c env nbIt = let env' = evalCommand c env in
  let (envTrue,envFalse) = evalBoolExpression b env' in
 (* let _ = print_string ("TTTr="^(printCst (getEnv "x@1" envTrue))^"\n") in *)
  let env'' = if (nbIt < widen) then
		envJoin env env'
  	      else 
        	envWiden env env'
  in if (envSseq env'' env) then  
	   envFalse
     else 
  		fixPoint b c env'' (if (nbIt >= widen) then 0 else (nbIt+1)) 
*)

and exactFixPoint b c env phis = 
 (* let _ = print_string ("T="^(printCst (getEnvComplete  "t@1" env (Empty,JEmpty)))^"\n") in *) 
let _ = flush stdout in 
	let env' = execWPhis phis env in
	let v = try execTest b env' with _ -> 0 in 
	let (envTrue,envFalse) = evalBoolExpression b env' 
    in (if (v>0) then 
		  let envTrue' = evalCommand c envTrue
		  in exactFixPoint b c envTrue' phis
        else (if (v<0) then  
           	    envFalse
			  else 
				envFalse (* raise (Error "Test must be exact in exactFixPoint") *)
			 ) 
	   ) ;;


(********************************************************************************************)

let rec envJoinPart env1 env2 = match env1 with
  [] -> env2
| (id,Cst(c1,l1))::env1' -> (try 
								let e' = getEnv id env2
						        in (match e' with
									  Cst(c2,l2) -> envJoinPart env1' (setEnv id (Cst(feJoin c1 c2,l1)) env2) 
									| _ -> envJoinPart env1' (setEnv id (Cst(c1,l1)) env2)
                                   )
				 		     with
						       _ -> envJoinPart env1' (setEnv id (Cst(c1,l1)) env2)
                            ) 
| (id,e)::env1' -> envJoinPart env1' (setEnv id e env2)
;;


let rec envPhiPartAux phis env1 env2 env = match phis with
  [] -> env
| Phi(id,id1,id2)::phis' -> let v1 = try getEnv id1 env1 with _ -> Cst((Empty,JEmpty),Lab(0)) in
                            let v2 = try getEnv id2 env2 with _ -> Cst((Empty,JEmpty),Lab(0)) in
                              (match (v1,v2) with
 								 (Cst(c1,_),Cst(c2,_)) -> let env' = setEnv id (Cst(feJoin c1 c2,Lab(0))) env 
							                              in envPhiPartAux phis' env1 env2 env' 
 							   | _ -> envPhiPartAux phis' env1 env2 env
							  ) ;;


let envPhiPart phis env1 env2 = let env' = envJoinPart env1 env2 in envPhiPartAux phis env1 env2 env' ;;


let rec exprEnv2ValEnv env = match env with
  [] -> []
| (id,Cst(c,l))::env' -> (id,c)::(exprEnv2ValEnv env')
| (id,_)::env' -> (exprEnv2ValEnv env') ;;


let rec valEnv2ExprEnv env = match env with
  [] -> []
| (id,v)::env' -> (id,Cst(v,Lab(0)))::(valEnv2ExprEnv env') ;;


let rec evalPartExpr e env bl = match e with
  Cst(c,_)        -> e
| Id(x,l)    	  -> if (memberString x bl) then e else
				     (try
						let e' = getEnv x env 
						in (match e' with
                        		Cst((I(aa,bb),J(cc,dd)),l') -> if (aa=bb) then e' else e
							|   Id(z,_) -> e'
							|   _ -> e
                           )  
                      with _ -> e)
| Plus(Cst(c1,_),Cst(c2,_),l)   -> let v = evalExprExact e (exprEnv2ValEnv env) 
                                   in Cst(v,l)
| Plus(e1,e2,l) -> let e1' = evalPartExpr e1 env bl in
                   let e2' = evalPartExpr e2 env bl in (Plus(e1',e2',l))
| Times(Cst(c1,_),Cst(c2,_),l)   -> let v = evalExprExact e (exprEnv2ValEnv env)
                                   in Cst(v,l)
| Times(e1,e2,l) -> let e1' = evalPartExpr e1 env bl in
                   let e2' = evalPartExpr e2 env bl in (Times(e1',e2',l))
| Minus(Cst(c1,_),Cst(c2,_),l)   -> let v = evalExprExact e (exprEnv2ValEnv env)
                                    in Cst(v,l)
| Minus(e1,e2,l) -> let e1' = evalPartExpr e1 env bl in
                    let e2' = evalPartExpr e2 env bl in (Minus(e1',e2',l))
| Div(Cst(c1,_),Cst(c2,_),l)   -> let v = evalExprExact e (exprEnv2ValEnv env)
                                  in Cst(v,l)
| Div(e1,e2,l) -> let e1' = evalPartExpr e1 env bl in
                    let e2' = evalPartExpr e2 env bl in (Div(e1',e2',l))
| Uminus(Cst(c1,_),l)    -> let v = evalExprExact e (exprEnv2ValEnv env)
                            in Cst(v,l)
| Uminus(e1,l) -> Uminus(evalPartExpr e1 env bl,l)
| Sqrt(Cst(c1,_),l)    -> let v = evalExprExact e (exprEnv2ValEnv env)
                            in Cst(v,l)
| Sqrt(e1,l) -> Sqrt(evalPartExpr e1 env bl,l)

;;


let rec evalPartExprBool e env bl = match e with
  BCst(_) -> e
| Lt(e1,e2,l) -> Lt(evalPartExpr e1 env bl,evalPartExpr e2 env bl,l)
| Gt(e1,e2,l) -> Lt(evalPartExpr e1 env bl,evalPartExpr e2 env bl,l)
| Eq(e1,e2,l) -> Lt(evalPartExpr e1 env bl,evalPartExpr e2 env bl,l)
;;


let rec evalPartCmd c env blackList = match c with
  Assign(id,e,l) -> let e' = evalPartExpr e env blackList in
						(match e' with
						   Cst(x,l') -> (Assign(id,e',l),setEnv id e' env,blackList) 
						 | Id(x,l') -> (Assign(id,e',l),setEnv id e' env,blackList) 
						 | _ -> (Assign(id,e',l),env,blackList)
						)
| Nop(l) -> (c,env,blackList)
| Seq(c1,c2,l) -> let (c1',env',blackList') = evalPartCmd c1 env blackList in
				  let (c2',env'',blackList'') = evalPartCmd c2 env' blackList' in
					(Seq(c1',c2',l),env'',blackList'')
| Cond(b,c1,c2,phis,l) -> let (c1',env',blackList') = evalPartCmd c1 env blackList in
				          let (c2',env'',blackList'') = evalPartCmd c2 env' blackList in
					      let env''' = envPhiPart phis env' env'' in
							(Cond(b,c1',c2',phis,l),env''',blackList'@blackList'')
| While(b,c',phis,l) ->  let (c'',env',blackList') = evalPartCmd c' env ((getPhiVars phis)@blackList) in
							(While(b,c'',phis,l),env',blackList')
;;
	

let rec isDefE e env = match env with
  [] -> false
| (e',_)::env' -> if (eqExpr e e') then true else isDefE e env' ;;


let rec getEnvExpr e env = match env with
  [] -> raise (Error("impossible in getEnvExpr"))
| (e',v)::env' -> if (eqExpr e e') then v else getEnvExpr e env' ;;


let rec setEnvExpr id v env = 
match env with
  [] -> (id,v)::[]
| (id',v')::env' -> if (eqExpr id id') then (id,v)::env' else (id',v')::(setEnvExpr id v env') ;;


let rec cleanTmpVarsExpr e exprIdEnv = if (isDefE e exprIdEnv) then
	getEnvExpr e exprIdEnv
  else
	(match e with
	   Id(_) -> e
	 | Cst(_) -> e
	 | Plus(e1,e2,l) -> Plus(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
	 | Minus(e1,e2,l) -> Minus(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
	 | Times(e1,e2,l) -> Times(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
	 | Div(e1,e2,l) -> Div(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
	 | Uminus(e1,l) -> Uminus(cleanTmpVarsExpr e1 exprIdEnv,l)
	 | Sqrt(e1,l) -> Sqrt(cleanTmpVarsExpr e1 exprIdEnv,l)
    )
;;


let rec cleanTmpVarsBoolExpr e exprIdEnv = match e with
  BCst(_) -> e
| Lt(e1,e2,l) -> Lt(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
| Gt(e1,e2,l) -> Gt(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
| Eq(e1,e2,l) -> Eq(cleanTmpVarsExpr e1 exprIdEnv,cleanTmpVarsExpr e2 exprIdEnv,l)
;;


let rec cleanTmpVars c exprIdEnv = match c with
  Assign(id,e,l) -> if (isDefE e exprIdEnv) then 
						(Nop(l),setEnvExpr (Id(id,l)) (getEnvExpr e exprIdEnv) exprIdEnv)
					else
						(Assign(id,cleanTmpVarsExpr e exprIdEnv,l),setEnvExpr e (Id(id,l)) exprIdEnv) 
| Nop(_) -> (c,exprIdEnv)
| Seq(c1,c2,l) -> let (c1',env') = cleanTmpVars c1 exprIdEnv in
				  let (c2',env'') = cleanTmpVars c2 env' in (Seq(c1',c2',l),env'')
| Cond(b,c1,c2,phis,l) -> 
				  let (c1',env') = cleanTmpVars c1 exprIdEnv in
				  let (c2',env'') = cleanTmpVars c2 env' in
				  let b' = cleanTmpVarsBoolExpr b exprIdEnv in
					(Cond(b,c1',c2',phis,l),env'')
| While(b,c1,phis,l) -> 
				  let (c1',env') = cleanTmpVars c1 exprIdEnv in
				  let b' = cleanTmpVarsBoolExpr b exprIdEnv in
					(While(b,c1',phis,l),env')
;;


let rec readVars e = match e with
  Id(x,_) -> [x]
| Cst(_) -> []
| Plus(e1,e2,l) -> (readVars e1)@(readVars e2)
| Minus(e1,e2,l) -> (readVars e1)@(readVars e2)
| Times(e1,e2,l) -> (readVars e1)@(readVars e2)
| Div(e1,e2,l) -> (readVars e1)@(readVars e2)
| Uminus(e1,l) -> (readVars e1)
| Sqrt(e1,l) -> (readVars e1)
;;


let readVarsBool b = match b with
  BCst(_) -> []
| Lt(e1,e2,_) -> (readVars e1)@(readVars e2)
| Gt(e1,e2,_) -> (readVars e1)@(readVars e2)
| Eq(e1,e2,_) -> (readVars e1)@(readVars e2)
;;


let rec readPhiVars phis = match phis with
  [] -> []
| Phi(_,a,b)::phis' -> a::b::(readPhiVars phis')
;;


let rec readVarsCmd c = match c with
  Assign(_,e,_) -> readVars e
| Nop(_) -> []
| Seq(c1,c2,_) -> (readVarsCmd c1)@(readVarsCmd c2)
| Cond(b,c1,c2,phis,_) -> (readVarsBool b)@(readVarsCmd c1)@(readVarsCmd c2)@(readPhiVars phis)
| While(b,c1,phis,_) -> (readVarsBool b)@(readVarsCmd c1)@(readPhiVars phis)
;;


let rec removeUnreadVars c bl = match c with
  Assign(id,_,l) -> if (memberString id bl) then c else Nop(l) 
| Nop(_) -> c
| Seq(c1,c2,l) -> Seq(removeUnreadVars c1 bl,removeUnreadVars c2 bl,l)
| Cond(b,c1,c2,phis,l) -> Cond(b,removeUnreadVars c1 bl,removeUnreadVars c2 bl,phis,l)
| While(b,c1,phis,l) -> While(b,removeUnreadVars c1 bl,phis,l)
;;


