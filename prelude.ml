open SalsaTypes ;;
open Num ;;

(**********************************************************************************)
(********************************** Values     ************************************)
(**********************************************************************************)

let debugMode = ref true ;;

let errPrint s = if (!debugMode) then (print_string s; flush stdout) else () ;;

let string_of_float f = Printf.sprintf "%.15f" f ;;

let codeLine =  ref 1 ;;
let widen = max_int ;;

let maxLevelEquivExprs = ref 2 ;;

let cpt = ref 0 ;;
let sliceSize = ref 50 ;;

let stat = Lab(-1) ;;
let dyn  = Lab(1) ;;


let globalEnv = ref [("type gen",(Empty,JEmpty))] ;;
let dynVars = ref ["type gen"] ;;


exception Error of string ;;
exception GetEnvError of string ;;


let newVar () = let _ = cpt := !cpt+1 in ("TMP_"^(string_of_int !cpt)) ;; 


let rec memberString s l = match l with
  [] -> false
| x::xs -> if (((String.compare x s)=0) ) 
           then true else memberString s xs ;;

let funList = ref [Func("type gen", [], Nop(Lab(0)), "type gen", Lab(0)) ]  ;;

let funListTransform = ref [Func("type gen", [], Nop(Lab(0)), "type gen", Lab(0)) ] ;;

let _ = funListTransform:=[] ;;


(**********************************************************************************)
(*************************** Transformation Tools *********************************)
(**********************************************************************************)


let removeSSA id = 
  try
    let i = String.index id '@'  
    in String.sub id 0 i 
  with _ -> id ;;


let isUselessAssign c = match c with
  Assign(x,Id(y,_),_) -> (String.compare x y)=0
| _ -> false ;;


let rec isNop c = match c with
  Assign(_) -> isUselessAssign c
| Seq(c1,c2,_) -> (isNop c1) && (isNop c2)
| Nop(_) -> true
| Cond(_,c1,c2,phis,_) -> (isNop c1) && (isNop c2)
| While(_,c1,phis,_) -> isNop c1 
;;


let rec removeSSAExpr e = match e with
  Id(x,l)        -> Id(removeSSA x,l)
| Cst(_)         -> e
| Plus(e1,e2,l)  -> Plus(removeSSAExpr e1,removeSSAExpr e2,l)
| Minus(e1,e2,l) -> Minus(removeSSAExpr e1,removeSSAExpr e2,l)
| Times(e1,e2,l) -> Times(removeSSAExpr e1,removeSSAExpr e2,l)
| Div(e1,e2,l)   -> Div(removeSSAExpr e1,removeSSAExpr e2,l)
| Uminus(e1,l)   -> Uminus(removeSSAExpr e1,l)
| Sqrt(e1,l)     -> Sqrt(removeSSAExpr e1,l)
| Cos(e1,l)      -> Cos(removeSSAExpr e1,l)
| Sin(e1,l)      -> Sin(removeSSAExpr e1,l)
| Exp(e1,l)      -> Exp(removeSSAExpr e1,l)
| Log(e1,l)      -> Log(removeSSAExpr e1,l)
| IntOfBool(be,l)-> IntOfBool(removeSSABExpr be,l) 
| FunCall(_)     -> e 


and removeSSABExpr e = match e with
  BCst(_)     -> e
| Eq(e1,e2,l) -> Eq(removeSSAExpr e1,removeSSAExpr e2,l)
| Lt(e1,e2,l) -> Lt(removeSSAExpr e1,removeSSAExpr e2,l)
| Lte(e1,e2,l)-> Lte(removeSSAExpr e1,removeSSAExpr e2,l)
| Gt(e1,e2,l) -> Gt(removeSSAExpr e1,removeSSAExpr e2,l)
| Gte(e1,e2,l)-> Gte(removeSSAExpr e1,removeSSAExpr e2,l)
| And(e1,e2,l)-> And(removeSSABExpr e1,removeSSABExpr e2,l)
| Or(e1,e2,l) -> Or(removeSSABExpr e1,removeSSABExpr e2,l)
| Not(e1,l)   -> Not(removeSSABExpr e1,l)
| BoolOfInt(e1,l) -> BoolOfInt(removeSSAExpr e1,l) 
;;


let rec removeSSACmd c = match c with
  Assign(id,e,l) -> let c' = Assign(removeSSA id,removeSSAExpr e,l) 
                    in if (isUselessAssign c') then Nop(l) else c' 
| Seq(c1,c2,l) -> let c1' = removeSSACmd c1 in
                  let c2' = removeSSACmd c2 in
                    if (isNop c1') then c2' else
                    if (isNop c2') then c1' else Seq(c1',c2',l)
| Nop(_) -> c
| Cond(be,c1,c2,phis,l) -> let c1' = removeSSACmd c1 in
                      let c2' = removeSSACmd c2 in
                      let be' = removeSSABExpr be in Cond(be',c1',c2',phis,l)
| While(be,c1,phis,l) -> let c1' = removeSSACmd c1 in
                    let be' = removeSSABExpr be in While(be',c1',phis,l)
;;


(**********************************************************************************)
(********************************** Value Tools ***********************************)
(**********************************************************************************)
  

let getI v = match v with
  (I(c,d),_) -> (c,d)
| _ -> raise (Error "Should not happen in getI") ;;


let rec listMeet l1 l2 = match l1 with
  [] -> []
| x::xs -> if (List.mem x l2) then 
                x::(listMeet xs l2)
           else
                listMeet xs l2 ;;


let isZero e = match e  with
  Cst((I(a,b),J(c,d)),_) -> if ((a=0.0) && (b=0.0) && (c =0.0) && (d =0.0)) then true else false 
| _ -> false ;;


let isOne e = match e with
  Cst((I(a,b),J(c,d)),_) -> if ((a=1.0) && (b=1.0) && (c =0.0) && (d =0.0)) then true else false 
| _ -> false ;;


let isTwo e = match e with
  Cst((I(a,b),J(c,d)),_) -> if ((a=2.0) && (b=2.0) && (c =0.0) && (d =0.0)) then true else false 
| _ -> false ;;


let isSingleton (a,b) = match a with
  I(u,v) -> if (u=v) then true else false
| _ -> false ;;


let zero = Cst((I(0.0,0.0),J(0.0,0.0)),Lab(-1)) ;;


let one = Cst((I(1.0,1.0),J(0.0,0.0)),Lab(-1)) ;;

let minus_one = Cst((I(-1.0,-1.0),J(0.0,0.0)),Lab(-1)) ;;


let two = Cst((I(2.0,2.0),J(0.0,0.0)),Lab(-1)) ;;


let maxErr = (I(0.0,0.0),J(infinity,neg_infinity)) ;;


let fZero = I(0.0,0.0) ;;
let rZero = J(0.0,0.0) ;;


(**********************************************************************************)
(********************************* Environments ***********************************)
(**********************************************************************************)


let rec joinLists l1 l2 = match l1 with
			   []   -> l2
	 		 | t::q -> if (not (List.mem t l2)) then joinLists q (t::l2) else joinLists q l2 ;;


let rec isDef id env = match env with
  [] -> false
| (id',_)::env' -> if ((String.compare id id')=0) then true else (isDef id env') ;;


let rec getEnv id env = match env with
  [] -> raise (GetEnvError id)  (* ne pas mettre autre chose a la place de id, il est recupere ds rewrite *) 
| (id',v)::env' -> if (id = id') then v else (getEnv id env') ;;


let rec getEnvComplete id env e = match env with
  [] -> e
| (id',v)::env' -> if (id=id') then v else (getEnvComplete id env' e) ;;


let rec setEnv id v env = match env with
  [] -> (id,v)::[]
| (id',v')::env' -> if (id=id') then (id,v)::env' else (id',v')::(setEnv id v env') ;;


let rec getDomain env = match env with 
  [] -> []
| (id,_)::env' -> id::(getDomain env') ;;


(**********************************************************************************)
(********************************* Rationnals   ***********************************)
(**********************************************************************************)


let r_of_f_aux2 s = 
  let l = String.length s in
  let i = String.index s '.' in
  let e = try
                          String.index s 'e'
                    with Not_found -> l
  in
  let nume = (String.sub s 0 i)^(String.sub s (i+1) (e-i-1)) in
  let valNume = num_of_string nume in
  let deno1 = "1"^(String.make (e-i-1) '0') in
  let valDeno1 = num_of_string deno1 in
  let valMantissa = div_num valNume valDeno1 in
    if (e<l) then
      let exp = String.sub s (e+1) (l-e-1) in
      let valExp = if ((String.sub exp 0 1)="+") then
              (int_of_string (String.sub exp 1 ((String.length exp)-1)))
      else
              (int_of_string exp)
      in
        if (valExp < 0) then
          let valExp2 = abs valExp in
          let deno2   = "1"^(String.make valExp2 '0') in
            div_num   valMantissa (num_of_string deno2)
        else
            let mult  = "1"^(String.make valExp '0') in
            mult_num  valMantissa (num_of_string mult)
    else
      valMantissa ;;


let r_of_f_aux x =
  if (x < 0.0) then
    let x' = r_of_f_aux2 (string_of_float (abs_float x)) in
      x' */ (num_of_int (-1))
  else
    r_of_f_aux2 (string_of_float x) ;; 


(**********************************************************************************)
(********************************* Expressions  ***********************************)
(**********************************************************************************)


let isCst c = match c with
  (I(a,b),_) -> if (a=b) then true else false
| _ -> false ;;


let eqJCst j j' = match (j,j') with
  (J(a,b),J(a',b')) -> ((a = a') && (b = b'))
| (JInfty,JInfty) -> true
| (JEmpty,JEmpty) -> true
| _ -> false ;;


let eqCst a b = match (a,b) with
  ((I(a,b),j),(I(a',b'),j')) -> (a = a') && (b = b') && (eqJCst j j') 
| ((Empty,_),(Empty,_)) -> true
| _ -> false ;;


let rec eqExprWeak e1 e2 n = 
  if (n > (!maxLevelEquivExprs)) then true
  else match (e1,e2) with
  (Cst(a,_),Cst(b,_)) -> true (* eqCst a b *)
| (Id(x,_),Id(y,_)) -> true (* (String.compare x y)==0 *)
| (Plus(e1',e2',_),Plus(e1'',e2'',_)) -> (eqExprWeak e1' e1'' (n+1)) && (eqExprWeak e2' e2'' (n+1))
| (Minus(e1',e2',_),Minus(e1'',e2'',_)) -> (eqExprWeak e1' e1'' (n+1)) && (eqExprWeak e2' e2'' (n+1))
| (Times(e1',e2',_),Times(e1'',e2'',_)) -> (eqExprWeak e1' e1'' (n+1)) && (eqExprWeak e2' e2'' (n+1))
| (Div(e1',e2',_),Div(e1'',e2'',_)) -> (eqExprWeak e1' e1'' (n+1)) && (eqExprWeak e2' e2'' (n+1))
| (Uminus(e1',_),Uminus(e1'',_)) -> (eqExprWeak e1' e1'' (n+1)) 
| _ -> false ;;


let rec eqExpr e1 e2 = match (e1,e2) with
  (Cst(a,l),Cst(b,l')) -> (eqCst a b) && (l=l')
| (Id(x,_),Id(y,_)) -> (String.compare x y) = 0
| (Plus(e1',e2',_),Plus(e1'',e2'',_)) -> (eqExpr e1' e1'') && (eqExpr e2' e2'')
| (Minus(e1',e2',_),Minus(e1'',e2'',_)) -> (eqExpr e1' e1'') && (eqExpr e2' e2'')
| (Times(e1',e2',_),Times(e1'',e2'',_)) -> (eqExpr e1' e1'') && (eqExpr e2' e2'')
| (Div(e1',e2',_),Div(e1'',e2'',_)) -> (eqExpr e1' e1'') && (eqExpr e2' e2'')
| (Uminus(e1',_),Uminus(e1'',_)) -> (eqExpr e1' e1'') 
| _ -> false ;;


let rec eqBoolExpr b1 b2 = match (b1,b2) with
  (BCst(b,_),BCst(b',_))       -> b = b'
| (Eq(e1,e2,_),Eq(e1',e2',_))  -> (eqExpr e1 e1') && (eqExpr e2 e2')
| (Lt(e1,e2,_),Lt(e1',e2',_))  -> (eqExpr e1 e1') && (eqExpr e2 e2')
| (Lte(e1,e2,_),Lte(e1',e2',_))-> (eqExpr e1 e1') && (eqExpr e2 e2') 
| (Gt(e1,e2,_),Gt(e1',e2',_))  -> (eqExpr e1 e1') && (eqExpr e2 e2') 
| (Gte(e1,e2,_),Gte(e1',e2',_))-> (eqExpr e1 e1') && (eqExpr e2 e2') 
| (And(e1,e2,_),And(e1',e2',_))-> (eqBoolExpr e1 e1') && (eqBoolExpr e2 e2') 
| (Or(e1,e2,_),Or(e1',e2',_))  -> (eqBoolExpr e1 e1') && (eqBoolExpr e2 e2') 
| (Not(e1,_),Not(e1',_))       -> (eqBoolExpr e1 e1')  
| (BoolOfInt(e1,_),BoolOfInt(e2,_)) -> (eqExpr e1 e2) 
| _ -> false ;;


let rec eqCmd c1 c2 = match (c1,c2) with
  (Assign(id1,e1,_),Assign(id2,e2,_)) -> ((String.compare id1 id2)=0) && (eqExpr e1 e2) 
| (Seq(Nop(_),c1',_),c2) -> eqCmd c1' c2
| (c1,Seq(Nop(_),c2',_)) -> eqCmd c1 c2'
| (Seq(c1,c2,_),Seq(c1',c2',_)) -> (eqCmd c1 c1') && (eqCmd c2 c2')
| (Cond(b,c1,c2,phis,_),Cond(b',c1',c2',phis',_)) -> (eqBoolExpr b b') && (eqCmd c1 c1') && (eqCmd c2 c2')
| (While(b,c,phis,_),While(b',c',phis',_)) -> (eqBoolExpr b b') && (eqCmd c c') 
| (Nop(_),Nop(_)) -> true
| _ -> false ;;


let rec memberExpr (e,err) l = match l with
  [] -> false
| (e',_)::l' -> if (eqExpr e e') then true else memberExpr (e,err) l' ;;


let rec memberEnv (c,env) l = match l with
  [] -> false
| (c',env')::l' -> if ((eqCmd (removeSSACmd c) (removeSSACmd c')) (* && 
					   (eqEnvs env env') *)
					  ) then true else memberEnv (c,env) l' ;;
	

let rec memberEnvStrict (c,env,measure) l = match l with
  [] -> false
| (c',env',measure')::l' -> if (eqCmd (removeSSACmd c) (removeSSACmd c')) then true else memberEnvStrict (c,env,measure) l' ;;


let rec getExprVar e = match e with
  Id(x,_)        -> [x]
| Cst(_)         -> []
| Plus(e1,e2,_)  -> (getExprVar e1)@(getExprVar e2)
| Minus(e1,e2,_) -> (getExprVar e1)@(getExprVar e2)
| Times(e1,e2,_) -> (getExprVar e1)@(getExprVar e2)
| Div(e1,e2,_)   -> (getExprVar e1)@(getExprVar e2)
| Uminus(e1,_)   -> (getExprVar e1) 
| Sqrt(e1,_)     -> (getExprVar e1) 
| Cos(e1,_)      -> (getExprVar e1) 
| Sin(e1,_)      -> (getExprVar e1) 
| Exp(e1,_)      -> (getExprVar e1) 
| Log(e1,_)      -> (getExprVar e1) 
| IntOfBool(be,_) -> (getBExprVar be)


and getBExprVar e = match e with
  BCst(_)      -> []
| Eq(e1,e2,_)  -> (getExprVar e1)@(getExprVar e2)
| Lt(e1,e2,_)  -> (getExprVar e1)@(getExprVar e2) 
| Lte(e1,e2,_) -> (getExprVar e1)@(getExprVar e2) 
| Gt(e1,e2,_)  -> (getExprVar e1)@(getExprVar e2) 
| Gte(e1,e2,_) -> (getExprVar e1)@(getExprVar e2) 
| And(e1,e2,_) -> (getBExprVar e1)@(getBExprVar e2) 
| Or(e1,e2,_)  -> (getBExprVar e1)@(getBExprVar e2) 
| Not(e1,_)    -> (getBExprVar e1) 
| BoolOfInt(e1,_) -> (getExprVar e1) 
;;


let rec getPhiVars phis = match phis with
  [] -> []
| Phi(_,y,z)::phis' -> y::z::(getPhiVars phis') ;;


let rec getFunc idf fl = match fl with 
   []    ->  raise (Error ("Function Does Not Exist "^idf))
 | (Func(name, args, c, return, lab))::fs ->  if ((String.compare name idf) = 0 ) then  (Func(name, args, c, return, lab))
                                               else getFunc idf fs ;;


