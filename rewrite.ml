


open SalsaTypes ;;
open Prelude ;;
open Ssa ;;
open Num ;;
open Float ;;
open Print;;
open MainEPEG ;;



let rec cmd2ctx c = match c with
  Nop(_) -> CtxNop
| Assign(id,e,_) -> CtxAssign(id,e)
| Seq(c1,c2,_) -> CtxSeq(cmd2ctx c1,cmd2ctx c2)
| Cond(be,c1,c2,phis,_) -> CtxCond(be,cmd2ctx c1,cmd2ctx c2,phis) 
| While(be,c1,phis,_) -> CtxWhile(be,cmd2ctx c1,phis) 
;;

let rec insertCmdInCtx cmd ctx = match ctx with
  Hole -> cmd
| CtxNop -> Nop(Lab(0))
| CtxAssign(id,e) -> Assign(id,e,Lab(0))
| CtxSeq(ctx1,ctx2) -> Seq(insertCmdInCtx cmd ctx1,insertCmdInCtx cmd ctx2,Lab(0))  
| CtxCond(be,ctx1,ctx2,phis) -> Cond(be,insertCmdInCtx cmd ctx1,insertCmdInCtx cmd ctx2,phis,Lab(0)) 
| CtxWhile(be,ctx1,phis) -> While(be,insertCmdInCtx cmd ctx1,phis,Lab(0)) 
;;


let rec insertCtx1InCtx2 ctx1 ctx2 = match ctx2 with
  Hole -> ctx1
| CtxNop -> ctx2
| CtxAssign(id,e) -> ctx2
| CtxSeq(ctx,ctx') -> CtxSeq(insertCtx1InCtx2 ctx1 ctx,insertCtx1InCtx2 ctx1 ctx')  
| CtxCond(be,ctx,ctx',phis) -> CtxCond(be,insertCtx1InCtx2 ctx1 ctx,insertCtx1InCtx2 ctx1 ctx',phis)
| CtxWhile(be,ctx,phis) -> CtxWhile(be,insertCtx1InCtx2 ctx1 ctx,phis) 
;;



let rec meanErrorAux env idList res = match idList with
  [] -> res
| id::ids -> meanErrorAux env ids (fePlus res (getEnv id env)) ;;


let meanError c env = 
	let idList = getAssignedVars c in
	  meanErrorAux env idList (I(0.0,0.0),J(0.0,0.0)) ;;


let rec joinStateLists l1 l2 = match l1 with
  [] -> l2
| t::q -> if (memberEnv t l2) then
            joinStateLists q l2  
          else 
            joinStateLists q (t::l2) 
;;


let rec exprSize e = match e with
  Id(x,_) -> 1
| Cst(_) -> 1
| Plus(e1,e2,l)   -> 1 + (max (exprSize e1) (exprSize e2)) 
| Minus(e1,e2,l)  -> 1 + (max (exprSize e1) (exprSize e2))
| Times(e1,e2,l)  -> 1 + (max (exprSize e1) (exprSize e2))
| Div(e1,e2,l)    -> 1 + (max (exprSize e1) (exprSize e2))
| Uminus(e1,l)    -> 1 + (exprSize e1)  
| Sqrt(e1,l)      -> 1 + (exprSize e1)  
| Cos(e1,l)       -> 1 + (exprSize e1)  
| Sin(e1,l)       -> 1 + (exprSize e1)  
| Exp(e1,l)       -> 1 + (exprSize e1)  
| Log(e1,l)       -> 1 + (exprSize e1)  
| IntOfBool(e1,l) -> 1   
;;


let rec substVars e env s = match e with
   Id(x,_) -> (try 
                let e' = (getEnv x env) in
			    (e',s) (* virer s ds toute la fct *) 
               with _ -> (e,s+1)
			  )
| Cst(_)  		-> (e,s+1)
| Plus(e1,e2,l) 	-> let (e1',size1) = substVars e1 env s in
		   	   let (e2',size2) = substVars e2 env size1 in
		             (Plus(e1',e2',l),size2+1)
| Minus(e1,e2,l) 	-> let (e1',size1) = substVars e1 env s in
		           let (e2',size2) = substVars e2 env size1 in
			     (Minus(e1',e2',l),size2+1)
| Times(e1,e2,l) 	-> let (e1',size1) = substVars e1 env s in
		           let (e2',size2) = substVars e2 env size1 in
			     (Times(e1',e2',l),size2+1)
| Div(e1,e2,l) 		-> let (e1',size1) = substVars e1 env s in
		  	   let (e2',size2) = substVars e2 env size1 in
		             (Div(e1',e2',l),size2+1)
| Uminus(e1,l) 		-> let (e1',size1) = substVars e1 env s in
		             (Uminus(e1',l),size1+1)
| Sqrt(e1,l) 		-> let (e1',size1) = substVars e1 env s in
		             (Sqrt(e1',l),size1+1)
| Cos(e1,l) 		-> let (e1',size1) = substVars e1 env s in
		             (Cos(e1',l),size1+1)
| Sin(e1,l) 		-> let (e1',size1) = substVars e1 env s in
		             (Sin(e1',l),size1+1)
| Exp(e1,l) 		-> let (e1',size1) = substVars e1 env s in
	                     (Exp(e1',l),size1+1)
| Log(e1,l) 		-> let (e1',size1) = substVars e1 env s in
		             (Log(e1',l),size1+1)
| IntOfBool(e1,l) 	-> let (e1',size1) = substVarsBool e1 env s in
		             (IntOfBool(e1',l),size1+1)


and substVarsBool be env s = match be with
  BCst(_)      -> (be,s+1) 
| Lt(e1,e2,l)  -> let (e1',size1) = substVars e1 env s in
		  let (e2',size2) = substVars e2 env size1 in
		       (Lt(e1',e2',l),size2+1)
| Lte(e1,e2,l) -> let (e1',size1) = substVars e1 env s in
		  let (e2',size2) = substVars e2 env size1 in
		       (Lte(e1',e2',l),size2+1)
| Gt(e1,e2,l)  -> let (e1',size1) = substVars e1 env s in
		  let (e2',size2) = substVars e2 env size1 in
		       (Gt(e1',e2',l),size2+1)
| Gte(e1,e2,l) -> let (e1',size1) = substVars e1 env s in
		  let (e2',size2) = substVars e2 env size1 in
		       (Gte(e1',e2',l),size2+1)
| Eq(e1,e2,l)  -> let (e1',size1) = substVars e1 env s in
		  let (e2',size2) = substVars e2 env size1 in
		       (Eq(e1',e2',l),size2+1)
| And(e1,e2,l) -> let (e1',size1) = substVarsBool e1 env s in
		  let (e2',size2) = substVarsBool e2 env size1 in
		       (And(e1',e2',l),size2+1)
| Or(e1,e2,l)  -> let (e1',size1) = substVarsBool e1 env s in
		  let (e2',size2) = substVarsBool e2 env size1 in
		       (Or(e1',e2',l),size2+1)
| Not(e1,l)    -> let (e1',size1) = substVarsBool e1 env s in
		       (Not(e1',l),size1+1)
| BoolOfInt(e1,l)-> let (e1',size1) = substVars e1 env s in
		   (BoolOfInt(e1',l),size1+1)
;;



let rec addAssignments varList c env = match varList with
  [] -> c
| id::ids -> let e = try getEnv id env with _ -> Id(id,Lab(0)) in
			 let c' = Seq(Assign(id,e,Lab(0)),c,Lab(0))
			 in addAssignments ids c' env ;;


let rec removeVarsInEnv varList env = match env with
  [] -> []
| (id,v)::env' -> if (memberString id varList) then 
					removeVarsInEnv varList env'
				  else
					(id,v)::(removeVarsInEnv varList env') ;;


let rec mixListCondThen l1 be phis lab = match l1 with
  [] -> []
| (c',env)::l' -> (Cond(be,c',Nop(lab),phis,lab),env)::(mixListCondThen l' be phis lab) ;;


let rec mixListCondAux c l be phis lab = match l with
  [] -> []
| (c',env)::l' -> (Cond(be,c,c',phis,lab),env)::(mixListCondAux c l' be phis lab) ;;


let rec mixListCond l1 l2 be phis lab = match l1 with
  [] -> []
| (Nop(_),_)::l1' -> mixListCond l1' l2 be phis lab
| (c,_)::l1' -> (mixListCondAux c l2 be phis lab)@(mixListCond l1' l2 be phis lab) ;;


let rec mixListWhile l be phis lab = match l with
  [] -> []
| (c',env')::l' -> (While(be,c',phis,lab),env')::(mixListWhile l' be phis lab) 
;;


(****************************************************************************)


let rec addThenAssigns phis c l formalEnv = match phis with
  [] -> c
| Phi(x,y,z)::phis' -> 
	let e = try (getEnv y formalEnv) with _ -> Id(y,l)
	in addThenAssigns phis' (Seq(Assign(x,e,l),c,l)) l formalEnv ;;


let rec addElseAssigns phis c l formalEnv = match phis with
  [] -> c
| Phi(x,y,z)::phis' -> 
	let e = try (getEnv z formalEnv) with _ -> Id(z,l)
	in addElseAssigns phis' (Seq(Assign(x,e,l),c,l)) l formalEnv ;;


let rec formalEnvMerge env1 env2 = match env1 with
  [] -> env2
| (id,v)::env1' -> formalEnvMerge env1' (setEnv id v env2) ;;

(*
let affiche id (u,v) = print_string (id^"={"^(printFInterval u)^";"^(printRInterval v)^cc^"}") ;flush stdout;;
*)
let rec cleanAssignments c = match c with
  Nop(_) -> c
| Assign(id,Id(id',_),l) -> if ((String.compare id id')=0) then Nop(l) else c
| Assign(_) -> c
| Seq(Nop(_),c2,_) -> cleanAssignments c2
| Seq(c1,Nop(_),_) -> cleanAssignments c1
| Seq(c1,c2,l) -> Seq(cleanAssignments c1,cleanAssignments c2,l)
| Cond(be,c1,c2,phis,l) -> Cond(be,cleanAssignments c1,cleanAssignments c2,phis,l)
| While(be,c1,phis,l) -> While(be,cleanAssignments c1,phis,l) ;;


(*let rec printEnv env = match env with
  [] -> "\n"
| (id,v)::env' -> ("\n"^id^" = "^(printCst v)^(printEnv env')) ;; 
*)

let sardanaOnOneExpr id e l env ctx bl = 
	let anaCmd = cleanAssignments ((insertCmdInCtx (Assign(id,e,l)) ctx)) in
    let sardanaEnv = evalCommand anaCmd !globalEnv in
    let e' = evalPartExpr e (valEnv2ExprEnv sardanaEnv) bl in
	let (e'',_) = (match e' with
					  Sqrt(s,l') -> let (s',v) = transformExpression id s sardanaEnv in (Sqrt(s',l'),v)
					| Cos(s,l') -> let (s',v) = transformExpression id s sardanaEnv in (Cos(s',l'),v)
					| Sin(s,l') -> let (s',v) = transformExpression id s sardanaEnv in (Sin(s',l'),v)
					| Exp(s,l') -> let (s',v) = transformExpression id s sardanaEnv in (Exp(s',l'),v)
					| Log(s,l') -> let (s',v) = transformExpression id s sardanaEnv in (Log(s',l'),v)
					| _ -> transformExpression id e' sardanaEnv 
                  )
    in
	let c = Assign(id,e'',l) in
		(c,env) ;;


let rec sliceExpr e h seq env ctx =
  if (h < !sliceSize) then
    (match e with
      Id(_) 	    	-> (e,seq)
    | Cst(_)        	-> (e,seq)
    | Plus(e1,e2,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                     	   let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Plus(e1',e2',l),seq'')
    | Minus(e1,e2,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                           let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Minus (e1',e2',l),seq'')						 
    | Times(e1,e2,l)    -> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                           let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Times    (e1',e2',l),seq'')						 
    | Div(e1,e2,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                           let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Div(e1',e2',l),seq'')						 
    | Uminus(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Uminus(e1',l),seq')						
    | Sqrt(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Sqrt(e1',l),seq')						
    | Cos(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Cos(e1',l),seq')						 
    | Sin(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Sin(e1',l),seq')						 
    | Exp(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Exp(e1',l),seq')						 
    | Log(e1,l) 	-> let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (Log(e1',l),seq')		
    | IntOfBool(be1,l) 	-> let (be1',seq')  = sliceBoolExpr be1 (h+1) seq  env ctx in (IntOfBool(be1',l),seq')
    )
  else
    (match e with
      Id(_)         -> (e,seq)
    | Cst(_)	    -> (e,seq)
    | Plus(e1,e2,l) -> let id1 = newVar() in
		       let id2 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (e2',seq'') = sliceExpr e2 0 seq' env ctx in 
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in
                       let (ee2,c2) = (match e2' with 
                                        Id(id2',_) -> if ((String.compare (String.sub id2' 0 3) "TMP")=0) then 
										    		    (e2',Nop(l)) 
										       		  else (Id(id2,l),Assign(id2,e2',l))
									  | Cst(c,_) -> (e2',Nop(l))
                                      | _ -> (Id(id2,l),Assign(id2,e2',l))
                                     )
                       in (Plus(ee1,ee2,l),Seq(seq'',Seq(c1,c2,l),l))
    | Minus(e1,e2,l)-> let id1 = newVar() in
					   let id2 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (e2',seq'') = sliceExpr e2 0 seq' env ctx in 
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in
                       let (ee2,c2) = (match e2' with 
                                        Id(id2',_) -> if ((String.compare (String.sub id2' 0 3) "TMP")=0) then 
										    		    (e2',Nop(l)) 
										       		  else (Id(id2,l),Assign(id2,e2',l))
									  | Cst(c,_) -> (e2',Nop(l))
                                      | _ -> (Id(id2,l),Assign(id2,e2',l))
                                     )
                       in (Minus(ee1,ee2,l),Seq(seq'',Seq(c1,c2,l),l))
    | Times(e1,e2,l)-> let id1 = newVar() in
					   let id2 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (e2',seq'') = sliceExpr e2 0 seq' env ctx in 
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in
                       let (ee2,c2) = (match e2' with 
                                        Id(id2',_) -> if ((String.compare (String.sub id2' 0 3) "TMP")=0) then 
										    		    (e2',Nop(l)) 
										       		  else (Id(id2,l),Assign(id2,e2',l))
									  | Cst(c,_) -> (e2',Nop(l))
                                      | _ -> (Id(id2,l),Assign(id2,e2',l))
                                     )
                       in (Times(ee1,ee2,l),Seq(seq'',Seq(c1,c2,l),l))
    | Div(e1,e2,l) ->  let id1 = newVar() in
		       let id2 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (e2',seq'') = sliceExpr e2 0 seq' env ctx in 
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in
                       let (ee2,c2) = (match e2' with 
                                        Id(id2',_) -> if ((String.compare (String.sub id2' 0 3) "TMP")=0) then 
										    		    (e2',Nop(l)) 
										       		  else (Id(id2,l),Assign(id2,e2',l))
									  | Cst(c,_) -> (e2',Nop(l))
                                      | _ -> (Id(id2,l),Assign(id2,e2',l))
                                     )
                       in (Div(ee1,ee2,l),Seq(seq'',Seq(c1,c2,l),l))
    | Uminus(e1,l) ->  let id1 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Uminus(ee1,l),Seq(seq',c1,l))
    | Sqrt(e1,l) ->    let id1 = newVar() in
                       let (e1',seq') = sliceExpr e1 0 seq env ctx in
                       let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Sqrt(ee1,l),Seq(seq',c1,l))
    | Cos(e1,l) -> let id1 = newVar() in
                   let (e1',seq') = sliceExpr e1 0 seq env ctx in
                   let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Cos(ee1,l),Seq(seq',c1,l))
    | Sin(e1,l) -> let id1 = newVar() in
                   let (e1',seq') = sliceExpr e1 0 seq env ctx in
                   let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Sin(ee1,l),Seq(seq',c1,l))
    | Exp(e1,l) -> let id1 = newVar() in
                   let (e1',seq') = sliceExpr e1 0 seq env ctx in
                   let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Exp(ee1,l),Seq(seq',c1,l))
    | Log(e1,l) -> let id1 = newVar() in
                   let (e1',seq') = sliceExpr e1 0 seq env ctx in
                   let (ee1,c1) = (match e1' with 
                                     	Id(id1',_) -> if ((String.compare (String.sub id1' 0 3) "TMP")=0) then 
											    		   (e1',Nop(l)) 
												      else (Id(id1,l),Assign(id1,e1',l))
									  | Cst(c,_) -> (e1',Nop(l))
                                      | _ -> (Id(id1,l),Assign(id1,e1',l))
                                     )
                       in (Log(ee1,l),Seq(seq',c1,l))
  (*  | IntOfBool(e1,l) ->  let id1 = newVar() in
                          let (e1',seq') = sliceBoolExpr e1 0 seq env ctx in
                          let (ee1,c1) = (match e1' with 
                            		  BCst(_) -> (e1',Nop(l))
                                	 | _      -> Nop(l)
                                          )
                          in (IntOfBool(ee1,l),Seq(seq',c1,l))) *)
)


and sliceBoolExpr be h seq env ctx = (be,Nop(Lab(0)))  ;;
(*
  if (h < !sliceSize) then
    (match be with	    	
  BCst(_)               ->  (BCst(_,seq))
| Lt(e1,e2,l)  		->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Lt(e1',e2',l),seq'')
| Lte(e1,e2,l) 		->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Lte(e1',e2',l),seq'')
| Gt(e1,e2,l)  		->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Gt(e1',e2',l),seq'')
| Gte(e1,e2,l) 		->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Gte(e1',e2',l),seq'')
| Eq(e1,e2,l)  		->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceExpr e2 (h+1) seq' env ctx in (Eq(e1',e2',l),seq'')
| And(be1,be2,l) 	->  let (e1',seq')  = sliceBoolExpr be1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceBoolExpr be2 (h+1) seq' env ctx in (And(e1',e2',l),seq'') 
| Or(be1,be2,l)  	->  let (e1',seq')  = sliceBoolExpr be1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceBoolExpr be2 (h+1) seq' env ctx in (Or(e1',e2',l),seq'') 
| Not(be1,l)    	->  let (e1',seq')  = sliceBoolExpr be1 (h+1) seq  env ctx in
                            let (e2',seq'') = sliceBoolExpr be2 (h+1) seq' env ctx in (Not(e1',e2',l),seq'')
| BoolOfInt(e1,l)	->  let (e1',seq')  = sliceExpr e1 (h+1) seq  env ctx in (BoolOfInt(e1',l),seq')
)
*)


let buildAssignmentAux id e l formalEnv ctx bl =
let (e',_) = substVars e formalEnv 0 in
        if ((exprSize e') <= !sliceSize) then 
			 let (c,env) = sardanaOnOneExpr id e' l formalEnv ctx bl in (c,env)
        else 
          if ((String.compare (String.sub id 0 3) "TMP")=0) then
            (Assign(id,e',l),formalEnv)
          else
    	    let (e'',seq) = sliceExpr e' 0 (Nop(l)) formalEnv ctx in
            (Seq(seq,Assign(id,e'',l),l),formalEnv)
      ;;

 
let buildAssignment id e l formalEnv ctx bl =
match e with
  Id(x,_) ->
          if ((String.compare (String.sub x 0 3) "TMP")=0) then 
            (Assign(id,e,l),formalEnv)
          else 
            buildAssignmentAux id e l formalEnv ctx bl
| _ -> buildAssignmentAux id e l formalEnv ctx bl
;;
	  

let rec rewriteCommand c formalEnv ctx varRef blackList = 
match c with
   Nop(_) -> 
   (c,formalEnv)
 | Assign(_,FunCall(_), _) -> (c, formalEnv)   
 | Assign(id,e,l) ->
	if ((not (memberString id blackList)) && 
		((String.compare id varRef) != 0) && ((String.compare (String.sub id 0 3) "TMP")!=0) &&
        (List.length (listMeet (getExprVar e) (getDomain formalEnv)) = 0) 
       ) 
	then 
	 	(Nop(l),setEnv id e formalEnv)
    else
	 (try 
	  buildAssignment id e l formalEnv ctx blackList 
      with
        GetEnvError(id) ->  let c' = addAssignments [id] c formalEnv in
							let formalEnv' = removeVarsInEnv [id] formalEnv 
							in rewriteCommand c' formalEnv' ctx varRef (id::blackList)
     )
 | Seq(Nop(_),c2,_)	-> rewriteCommand c2 formalEnv ctx varRef blackList
 | Seq(c1,Nop(_),_) -> rewriteCommand c1 formalEnv ctx varRef blackList
 | Seq(Seq(c1,c2,l),c3,l') -> rewriteCommand (Seq(c1,Seq(c2,c3,l'),l)) formalEnv ctx varRef blackList
 | Seq(c1,c2,l) ->
					let (c1',env1) = rewriteCommand c1 formalEnv (insertCtx1InCtx2 (CtxSeq(Hole,cmd2ctx c2)) ctx) varRef blackList in
				   let ctx2 = insertCtx1InCtx2 (CtxSeq(cmd2ctx c1',Hole)) ctx in 
				   let (c2',env2) = rewriteCommand c2 env1 ctx2 varRef blackList in
						(Seq(c1',c2',l),env2)
 | Cond(be,c1,c2,phis,l) -> 
	let anaCmd = insertCmdInCtx c ctx in
	let v = try 
			    let sardanaEnv = evalCommand anaCmd !globalEnv 
				in execTest be sardanaEnv 
			with _ -> 0 
    in if (v>0) then 
			let blackList' = getAssignedVars c1 in
        	let (c1',env1') = rewriteCommand c1 formalEnv ctx varRef (blackList'@blackList) 
			in (addThenAssigns phis c1' l env1',env1')
        else (if (v<0) then 
				 let blackList' = getAssignedVars c2 in
 				 let (c2',env2') = rewriteCommand c2 formalEnv ctx varRef (blackList'@blackList)
				 in (addElseAssigns phis c2' l env2',env2')
   		      else 
 			    let beVars = getBExprVar be in
              	if (List.length (listMeet beVars (getDomain formalEnv)) = 0) then 
					let blackList' = (getAssignedVars c1) @ (getAssignedVars c2) in
                    let blackList'' = (getPhiVars phis) in
				    let (c1',env1') = rewriteCommand c1 formalEnv ctx varRef (blackList''@blackList'@blackList) in 
                    let (c2',env2') = rewriteCommand c2 formalEnv ctx varRef (blackList''@blackList'@blackList) in
					let (c',env') =(Cond(be,c1',c2',phis,l),formalEnvMerge env1' env2')
					in (addAssignments blackList'' c' formalEnv,env')
                else 
					let c'' = addAssignments beVars c formalEnv in
					let formalEnv' = removeVarsInEnv beVars formalEnv 
					in rewriteCommand c'' formalEnv' ctx varRef (beVars@blackList)
            )
 | While(be,c',phis,l) -> 
        let beVars = getBExprVar be in
		if (List.length (listMeet beVars (getDomain formalEnv)) = 0) then
			let phiVars = getWPhiBlackList phis in
			let bl = joinLists beVars phiVars in  
   		    let ctx' = insertCtx1InCtx2 (CtxWhile(be,Hole,phis)) ctx in
			let (c'',env') = rewriteCommand c' formalEnv ctx' varRef bl in
		    let c''' = While(be,c'',phis,l)
			in (c''',env')
		else 
			let phiVars = getWPhiBlackList phis in
			let bl = joinLists beVars phiVars in  		
			let c'' = addAssignments bl c formalEnv in
			let formalEnv' = removeVarsInEnv bl formalEnv 
			in rewriteCommand c'' formalEnv' ctx varRef (bl@blackList)
			
;;
 

let rec simplifyCmd c = match c with
	 	   
  Assign(id,Id(id',_),l) -> if ((String.compare id id')=0) then Nop(l) else c
	 	 
| Assign(id,e,l) -> c
	 	
| Seq(c1,c2,l) -> let c1' = simplifyCmd c1 in
	 	          let c2' = simplifyCmd c2 in
	 	          if (isNop c1') then c2' else
	 	          if (isNop c2') then c1' else Seq(c1',c2',l)

| Nop(_) -> c
	 	 
| Cond(be,c1,c2,phis,l) -> let c1' = simplifyCmd c1 in
	 	                   let c2' = simplifyCmd c2 in
	 	                         Cond(be,c1',c2',phis,l)
| While(be,c1,phis,l) -> let c1' = simplifyCmd c1
	 	                 in While(be,c1',phis,l)
;;
	 	 
	 	 
let rec memberCmd c cList = 
match cList with
 [] ->  false
| c':: cl' ->  if (eqCmd (removeSSACmd c') (removeSSACmd c)) then true else memberCmd c cl' ;;
	 	 
	 	
