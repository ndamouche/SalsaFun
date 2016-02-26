
open SalsaTypes ;;
open Num ;;
open Graphics ;; 
open Prelude ;;
open Float ;;
open Rewrite ;;
 


let rec evalExprQ e env = match  e with
  
  QCst(c,_)              	-> c 
| QId(x,_)    			-> getEnv x env
| QPlus(e1,e2,_)     		-> let v1 = evalExprQ e1 env in
				   let v2 = evalExprQ e2 env in  v1 +/ v2
| QMinus(e1,e2,_)     		-> let v1 = evalExprQ e1 env in 
				   let v2 = evalExprQ e2 env in v1 -/ v2 
| QTimes(e1,e2,_)  		-> let v1 = evalExprQ e1 env in 
 	        	  	   let v2 = evalExprQ e2 env in  v1 */ v2
| QDiv(e1,e2,_) 		-> let v1 = evalExprQ e1 env in
 				   let v2 = evalExprQ e2 env in v1 // v2
| QUminus(e1,_) 		-> let v1 = evalExprQ e1 env in (num_of_int 0) -/ v1 
| QSqrt(e1,_) 			-> let v1 = evalExprQ e1 env in  v1 **/ (r_of_f_aux 0.5) 
| QCos(e1,_) 			-> let v1 = evalExprQ e1 env in  v1 **/ (r_of_f_aux 0.5) 
| QSin(e1,_) 			-> let v1 = evalExprQ e1 env in  v1 **/ (r_of_f_aux 0.5) 
| QExp(e1,_) 			-> let v1 = evalExprQ e1 env in  (r_of_f_aux 2.71828182845904523536) **/ v1
(* | QLog(e1,_) 			-> let v1 = evalExprQ e1 env in  log v1 *) 
| QIntOfBool(be1,_) 		-> let v1 = evalBoolExpressionQ be1 env in if v1 then (num_of_int 1) else (num_of_int (-1))  


 and evalBoolExpressionQ b env = match b with  
 
  QBCst(b,_)				->  b
| QEq(e1,e2,_) 				->  let v1 = evalExprQ e1 env in
					    let v2 = evalExprQ e2 env in v1 =/ v2
| QLt(e1,e2,_)  			->  let v1 = evalExprQ e1 env in 
					    let v2 = evalExprQ e2 env in v1 </ v2
| QLte(e1,e2,_) 			->  let v1 = evalExprQ e1 env in 
				 	    let v2 = evalExprQ e2 env in v1 <=/ v2 		
| QGt(e1,e2,_)				->  let v1 = evalExprQ e1 env in
					    let v2 = evalExprQ e2 env in v1 >/ v2 
| QGte(e1,e2,_) 			->  let v1 = evalExprQ e1 env in
 					    let v2 = evalExprQ e2 env in v1 >=/ v2 
| QAnd(be1,be2,_) 			->  let v1 = evalBoolExpressionQ be1 env in
					    let v2 = evalBoolExpressionQ be2 env in v1 && v2
| QOr(be1,be2,_) 			->  let v1 = evalBoolExpressionQ be1 env in
					    let v2 = evalBoolExpressionQ be2 env in v1 || v2 
| QNot(be1,_) 				->  let v1 = evalBoolExpressionQ be1 env in if v1 then false  else true
| QBoolOfInt(e1,_)                      ->  let v1 = evalExprQ e1 env in if (v1 = num_of_int 1) then true  else false
| _ 					->  raise (Error ("evalBoolExpressionQ : case not yet implemented"))
;;


let rec evalQ c1 env c2 = match c1 with

  QNop(_)				-> (match c2 with
	   				     QNop(_) -> env
					    | _      -> evalQ c2 env c1
					  )

| QAssign(id,e,l)			-> let v = evalExprQ e env  in
					   let env' = setEnv id v env in
					      evalQ c2 env' (QNop(l))

| QCond(b,c1',c2',_) 		       -> let cond  = evalBoolExpressionQ b env 
				            in if cond then (evalQ c1' env c2) else (evalQ c2' env c2) 

| QSeq(c1',c2',l)			-> evalQ c1' env (QSeq(c2',c2,l))
        	   	       
| QWhile(b,c1',l)			-> let cond = evalBoolExpressionQ b env in
								   if cond then 
									 evalQ c1' env (QSeq(c1,c2,l))
								   else env 
;;



(************************************)


let rec evalExprF e env = match e with
  FCst(c,_)      		        ->  c 
| FId(x,_)    				-> getEnv x env 
| FPlus(e1,e2,_)   			-> let v1 = evalExprF e1 env in
                  			   let v2 = evalExprF e2 env in v1 +. v2
| FMinus(e1,e2,_)     		  	-> let v1 = evalExprF e1 env in 
     			                   let v2 = evalExprF e2 env in v1 -. v2 
| FTimes(e1,e2,_)  			-> let v1 = evalExprF e1 env in 
         			   	   let v2 = evalExprF e2 env in v1 *. v2    
| FDiv(e1,e2,_) 			-> let v1 = evalExprF e1 env in
 					   let v2 = evalExprF e2 env in v1 /. v2 		                    
| FUminus(e1,_) 			-> let v1 = evalExprF e1 env in 0.0 -. v1 
| FSqrt(e1,_) 				-> let v1 = evalExprF e1 env in sqrt v1 
| FCos(e1,_) 				-> let v1 = evalExprF e1 env in cos v1
| FSin(e1,_) 				-> let v1 = evalExprF e1 env in sin v1 
| FLog(e1,_) 				-> let v1 = evalExprF e1 env in log v1 
| FExp(e1,_) 				-> let v1 = evalExprF e1 env in exp v1
| FIntOfBool(be1,_) 			-> let v1 = evalBoolExpressionF be1 env in 
					     if v1 then (1.0) else (-1.0) 


and evalBoolExpressionF b env = match b with  
  
  FBCst(b,_)				->  b
| FEq(e1,e2,_) 				->  let v1 = evalExprF e1 env in
					    let v2 = evalExprF e2 env in v1 == v2
| FLt(e1,e2,_)  			->  let v1 = evalExprF e1 env in 
					    let v2 = evalExprF e2 env in v1 < v2 
| FLte(e1,e2,l) 			->  let v1 = evalExprF e1 env in 
					    let v2 = evalExprF e2 env in v1 <= v2 
| FGt(e1,e2,_)				->  let v1 = evalExprF e1 env in
					    let v2 = evalExprF e2 env in v1 > v2 
| FGte(e1,e2,l) 			->  let v1 = evalExprF e1 env in 
					    let v2 = evalExprF e2 env in v1 >= v2 
| FAnd(e1,e2,_) 			->  let v1 = evalBoolExpressionF e1 env in
					    let v2 = evalBoolExpressionF e2 env in v1 && v2
| FOr(e1,e2,_) 				->  let v1 = evalBoolExpressionF e1 env in
					    let v2 = evalBoolExpressionF e2 env in v1 || v2 
| FNot(e1,_) 				->  let v1 = evalBoolExpressionF e1 env in if v1 then false else true  
| FBoolOfInt(e1,_) 			->  let v1 = evalExprF e1 env in if (v1 = 1.0) then true else false  
| _     			        ->  raise (Error ("evalBoolExpressionF : case not yet implemented :-((("))

;;



let rec evalF c1 env c2 = match c1 with

  FNop(_)				-> (match c2 with
						FNop(_) -> env
					       | _      -> evalF c2 env c1
					   )
| FAssign(id,e,l)			-> let v = evalExprF e env  in
					   let env' = setEnv id v env in
					     evalF c2 env' (FNop(l))
| FCond(b,c1',c2',_) 			-> let cond  = evalBoolExpressionF b env 
					     in if cond then (evalF c1' env c2) else (evalF c2' env c2) 
| FSeq(c1',c2',l)			-> evalF c1' env (FSeq(c2',c2,l))
| FWhile(b,c1',l)			-> let cond = evalBoolExpressionF b env in
								   if cond then 
									 evalF c1' env (FSeq(c1,c2,l))
								   else env 
;;



(*************** Les flottants *********************)


let rec expr2FExpr e  = match e with 

  Cst((I(a,b),J(c,d)),l)  	 ->  FCst(a,l)
| Id(x,l)  		         ->  FId(removeSSA x,l) 
| Plus(e1,e2,l)  		 ->  let v1 = expr2FExpr e1  in
  				     let v2 = expr2FExpr e2  in FPlus(v1,v2,l) 
| Minus(e1,e2,l)		 ->  let v1 = expr2FExpr e1  in 
				     let v2 = expr2FExpr e2  in FMinus(v1,v2,l) 
| Times(e1,e2,l) 		 ->  let v1 = expr2FExpr e1  in
				     let v2 = expr2FExpr e2  in FTimes(v1,v2,l)  
| Div(e1,e2,l)			 ->  let v1 = expr2FExpr e1  in 
				     let v2 = expr2FExpr e2  in FDiv(v1,v2,l)
| Uminus(e1,l)    		 ->  let v1 = expr2FExpr e1  in FUminus(v1,l)
| Sqrt(e1,l)    		 ->  let v1 = expr2FExpr e1  in FSqrt(v1,l)
| Cos(e1,l) 			 ->  let v1 = expr2FExpr e1  in FCos(v1,l) 
| Sin(e1,l) 			 ->  let v1 = expr2FExpr e1  in FSin(v1,l) 
| Exp(e1,l) 			 ->  let v1 = expr2FExpr e1  in FExp(v1,l)
| Log(e1,l) 			 ->  let v1 = expr2FExpr e1  in FLog(v1,l)
| IntOfBool(be1,l) 		 ->  let v1 = bool2FBoolExpr be1 in FIntOfBool(v1,l)


and bool2FBoolExpr b = match b with  
 
  BCst(b,l)			->  FBCst(b,l)						
| Eq(e1,e2,l) 			->  let v1 = expr2FExpr e1 in
				    let v2 = expr2FExpr e2 in FEq(v1,v2,l)	
| Lt(e1,e2,l)  			->  let v1 = expr2FExpr e1 in 
				    let v2 = expr2FExpr e2 in FLt(v1,v2,l)
| Lte(e1,e2,l) 			->  let v1 = expr2FExpr e1 in
				    let v2 = expr2FExpr e2 in FLte(v1,v2,l) 
| Gt(e1,e2,l)			->  let v1 = expr2FExpr e1 in 
				    let v2 = expr2FExpr e2 in FGt(v1,v2,l) 
| Gte(e1,e2,l) 			->  let v1 = expr2FExpr e1 in
				    let v2 = expr2FExpr e2 in FGte(v1,v2,l) 
| And(e1,e2,l) 			->  let v1 = bool2FBoolExpr e1 in
				    let v2 = bool2FBoolExpr e2 in FAnd(v1,v2,l)
| Or(e1,e2,l) 			->  let v1 = bool2FBoolExpr e1 in 
				    let v2 = bool2FBoolExpr e2 in FOr(v1,v2,l) 
| Not(e1,l) 			->  let v1 = bool2FBoolExpr e1 in FNot(v1,l)
| BoolOfInt(e1,l)               ->  let v1 = expr2FExpr e1 in FBoolOfInt(v1,l) 
| _     			->  raise (Error ("bool2FBoolExpr : case not yet implemented :-((("))
;;


let rec cmd2FCmd c env = match c with

  Nop(l)			->  FNop(l) 
| Assign(id,e,l) 		->  FAssign(removeSSA id,expr2FExpr e,l) 
| Seq(c1,c2,l)     		->  let v1 = cmd2FCmd c1 env in
				    let v2 = cmd2FCmd c2 env in FSeq(v1,v2,l)
| Cond(b,c1,c2,_,l)		->  let bF = bool2FBoolExpr b in
				    let v1 = cmd2FCmd c1 env in 
			            let v2 = cmd2FCmd c2 env in FCond(bF,v1,v2,l)
| While(b,c1,_,l) 		->  let bF = bool2FBoolExpr b in
				    let v1 = cmd2FCmd c1 env in FWhile(bF,v1,l)
;;
  

(***************  Les rationnels  *********************)


let rec expr2QExpr e  = match e with 

  Cst((I(a,b),J(c,d)),l) 	 ->  (*if (a != b) then raise (Error "Mon dieu, a different de b!")
                                     else *) QCst(r_of_f_aux a,l) (*   !!!!!! may be not a *)
| Id(x,l)  			 ->  QId(removeSSA x,l) 
| Plus(e1,e2,l)  		 ->  let v1 = expr2QExpr e1  in
  				     let v2 = expr2QExpr e2  in QPlus(v1,v2,l) 
| Minus(e1,e2,l)		 ->  let v1 = expr2QExpr e1  in 
				     let v2 = expr2QExpr e2  in QMinus(v1,v2,l) 				
| Times(e1,e2,l) 		 ->  let v1 = expr2QExpr e1  in
				     let v2 = expr2QExpr e2  in QTimes(v1,v2,l)  
| Div(e1,e2,l)			 ->  let v1 = expr2QExpr e1  in 
				     let v2 = expr2QExpr e2  in QDiv(v1,v2,l)
| Uminus(e1,l)    		 ->  let v1 = expr2QExpr e1  in QUminus(v1,l)
| Sqrt(e1,l)    		 ->  let v1 = expr2QExpr e1  in QSqrt(v1,l)
| Cos(e1,l) 			 ->  let v1 = expr2QExpr e1  in QCos(v1,l) 
| Sin(e1,l) 			 ->  let v1 = expr2QExpr e1  in QSin(v1,l)
| Exp(e1,l) 			 ->  let v1 = expr2QExpr e1  in QExp(v1,l) 
| Log(e1,l) 			 ->  let v1 = expr2QExpr e1  in QLog(v1,l) 
| IntOfBool(be1,l) 		 ->  let v1 = bool2QBoolExpr be1 in QIntOfBool(v1,l) 


and bool2QBoolExpr b = match b with  
  
  BCst(b,l)			->  QBCst(b,l)
| Eq(e1,e2,l) 			->  let v1 = expr2QExpr e1  in
			            let v2 = expr2QExpr e2  in QEq(v1,v2,l)
| Lt(e1,e2,l)  			->  let v1 = expr2QExpr e1  in 
		 		    let v2 = expr2QExpr e2  in QLt(v1,v2,l)
| Lte(e1,e2,l)   		->  let v1 = expr2QExpr e1 in
				    let v2 = expr2QExpr e2 in QLte(v1,v2,l) 
| Gt(e1,e2,l)			->  let v1 = expr2QExpr e1  in 
			            let v2 = expr2QExpr e2  in QGt(v1,v2,l)
| Gte(e1,e2,l) 			->  let v1 = expr2QExpr e1 in
				    let v2 = expr2QExpr e2 in QGte(v1,v2,l) 
| And(e1,e2,l) 			->  let v1 = bool2QBoolExpr e1 in
				    let v2 = bool2QBoolExpr e2 in QAnd(v1,v2,l)
| Or(e1,e2,l) 			->  let v1 = bool2QBoolExpr e1 in 
				    let v2 = bool2QBoolExpr e2 in QOr(v1,v2,l) 
| Not(e1,l) 			->  let v1 = bool2QBoolExpr e1 in QNot(v1,l) 
| BoolOfInt(e1,l) 		->  let v1 = expr2QExpr e1 in QBoolOfInt(v1,l) 
| _     			->  raise (Error ("bool2QBoolExpr : case not yet implemented :-((("))

;;


let rec cmd2QCmd c env = match c with

  Nop(l)			->  QNop(l) 
| Assign(id,e,l) 		->  QAssign(removeSSA id,expr2QExpr e,l) 
| Seq(c1,c2,l)     		->  let v1 = cmd2QCmd c1 env in
				    let v2 = cmd2QCmd c2 env in QSeq(v1,v2,l)
| Cond(b,c1,c2,_,l)		->  let bF = bool2QBoolExpr b in
				    let v1 = cmd2QCmd c1 env in 
				    let v2 = cmd2QCmd c2 env in QCond(bF,v1,v2,l)
| While(b,c1,_,l) 		->  let bF = bool2QBoolExpr b in
				    let v1 = cmd2QCmd c1 env in QWhile(bF,v1,l)
;;


(**********************        ******************)


let nbTests = 50 ;;


let rec measureIterF cF a b step id fichierF varRef = 
 if a < b then
    let env = [(removeSSA id,a)] in  						(* l'env de depart à la ieme it *)
    let env'= evalF cF env (FNop(Lab(0))) in								(* l'env resultat de l'exec *)
    let res = getEnv varRef env' in 						(* le resulat de l'execution *)
    let fic = output_string fichierF ((string_of_float res)^"\n") in 
         measureIterF cF (a +. step) b step  id  fichierF varRef
 else () 
  

let measureF c gpFichier varRef =
  let env = !globalEnv in 
  let cF = cmd2FCmd c env in 
  let (id,(I(a,b),J(c,d))) = List.hd env in
  let step = (b -. a) /. (float_of_int nbTests) in
  let fichierF = open_out (gpFichier^".f") in
  let _ = measureIterF cF a b step id fichierF varRef in
  let _ = close_out fichierF in () ;;
  
  
  
let rec measureIterQ cQ a b step id fichierQ varRef = 
 if a </ b then
    let env = [(removeSSA id,a)] in  					    (* l'env de depart à la ieme it *)
    let env' = evalQ cQ env (QNop(Lab(0))) in				    (* l'env resultat de l'exec     *)
    let res = getEnv varRef env' in 			  		    (* le resulat de l'execution    *)
    let fic = output_string fichierQ ((approx_num_exp 11 res)^"\n") in 
         measureIterQ cQ (a +/ step) b step  id fichierQ varRef
 else ()
 
  


let measureQ c gpFichier varRef =
	let env = !globalEnv in 
	let cQ 	= cmd2QCmd c env in
	let (id,(I(a,b),J(c,d))) = List.hd env in
	let step = ((r_of_f_aux b) -/ (r_of_f_aux a)) // (num_of_int nbTests) in
 	let fichierQ = open_out (gpFichier^".q") in
 	let _ = measureIterQ cQ (r_of_f_aux a)  (r_of_f_aux b) step id fichierQ varRef in
   	let _ = close_out fichierQ in () ;;  
  

let rec measErrAux f q tf e1 e2 =
				let sf = input_line f in
				let vf = float_of_string sf in
				let sq = input_line q in
				let vq = float_of_string sq in
				let stf = input_line tf in
				let vtf = float_of_string stf in
				let err1 = abs_float (vq -. vf) in
				let err2 = abs_float (vq -. vtf) in
				let _ = output_string e1 ((string_of_float err1)^"\n") in
				let _ = output_string e2 ((string_of_float err2)^"\n") in
					measErrAux f q tf e1 e2
;;


let measErr () =
 			let f = open_in ((Array.get Sys.argv 1)^".f") in
 			let q = open_in ((Array.get Sys.argv 1)^".q") in
 			let tf = open_in ((Array.get Sys.argv 1)^"_t.f") in
 			let e1 = open_out ((Array.get Sys.argv 1)^".e1") in
 			let e2 = open_out ((Array.get Sys.argv 1)^".e2") in
			try measErrAux f q tf e1 e2
			with
				_ -> (close_out e1; close_out e2; close_in f; close_in q; close_in tf) ;;


let createGP fileName =  
(*    let gp = "/Applications/Gnuplot.app/Contents/Resources/bin/gnuplot" in *)
    let gp = "gnuplot" in 
    let gpFile  = open_out (fileName^".gp")    in
    let _ = output_string gpFile ("set grid\n") in
    let _ = output_string gpFile ("set term jpeg\n") in
    let _ = output_string gpFile ("set output \""^fileName^".jpeg\"\n") in
    let s1 = "\""^fileName^".f\" with impulses title \"Original "^fileName^": floating-point\"," in
    let s2 = "\""^fileName^".q\" with points title \"Original "^fileName^": rational\"," in
    let s3 = "\""^fileName^"_t.f\" with points title \"Salsa "^fileName^": floating-point\"," in
    let s4 = "\""^fileName^"_t.q\" with points title \"Salsa "^fileName^": rational\"" in
    let _ = output_string gpFile ("plot "^s1^s2^s3^s4) in
    let _ = close_out gpFile in
    let _ = Sys.command (gp^" "^fileName^".gp") in () ;;


let createGPErr fileName =  
(*    let gp = "/Applications/Gnuplot.app/Contents/Resources/bin/gnuplot" in *)
    let gp = "gnuplot" in 
    let gpFile  = open_out (fileName^"2.gp")    in
    let _ = output_string gpFile ("set grid\n") in
    let _ = output_string gpFile ("set term jpeg\n") in
    let _ = output_string gpFile ("set output \""^fileName^"_err.jpeg\"\n") in
    let s1 = "\""^fileName^".e1\" with points title \"Original Error "^fileName^"\"," in
    let s2 = "\""^fileName^".e2\" with points title \"New Error "^fileName^"\"" in
    let _ = output_string gpFile ("plot "^s1^s2) in
    let _ = close_out gpFile in
    let _ = Sys.command (gp^" "^fileName^"2.gp") in () ;;




(*   	let title   = set_window_title (filename)  in 
 	let    _    = open_graph  "512x384+50-40"  in
  	let    _ 	= close_graph  in () 
 ;;
*) 





  
