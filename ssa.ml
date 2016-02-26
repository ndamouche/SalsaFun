
open SalsaTypes ;;
open Prelude ;;

let ssaEnv = ref [("type_gen",0)] ;;

let ssaIzeLHS id = let i = try
                                 getEnv id !ssaEnv 
                           with _ -> 0
                   in
                        let _ = ssaEnv := setEnv id (i+1) !ssaEnv 
                        in (id^"@"^(string_of_int (i+1))) ;;

let ssaIzeRHS id = let i = try
                               getEnv id !ssaEnv
                           with _ -> 0 
                   in (id^"@"^(string_of_int i)) ;;


let rec getAssignedVars c = match c with
  Assign(id,_,_) -> [removeSSA id]
| Seq(c1,c2,_) -> joinLists (getAssignedVars c1) (getAssignedVars c2)
| Nop(_) -> []
| Cond(_,c1,c2,_,_) -> joinLists (getAssignedVars c1) (getAssignedVars c2)
| While(_,c,_,_) -> (getAssignedVars c)
;;


let rec getAssignedVarsExact c = match c with
  Assign(id,_,_) -> id::[]
| Seq(c1,c2,_) -> joinLists (getAssignedVarsExact c1) (getAssignedVarsExact c2)
| Nop(_) -> []
| Cond(_,c1,c2,_,_) -> joinLists (getAssignedVarsExact c1) (getAssignedVarsExact c2)
| While(_,c,_,_) -> (getAssignedVarsExact c)
;;


let rec searchInPhis id phis res = match phis with
  [] -> res
| Phi(id',id1,id2)::phis' -> if (String.compare id (removeSSA id')=0) then id' else searchInPhis id phis' res
;;


let rec searchLastAssign id c res = match c with
  Assign(id',_,_) 		-> if (String.compare id (removeSSA id')=0) then id' else res
| Seq(c1,c2,_)		 	-> searchLastAssign id c2 (searchLastAssign id c1 res)
| Nop(_) 				-> res
| Cond(_,c1,c2,phis,_) 	-> searchInPhis id phis res
| While(_,c,phis,_) 	-> searchInPhis id phis res
;;


let rec searchFirstAssign id c res = match c with
  Assign(id',_,_) 		-> if (String.compare id (removeSSA id')=0) then 
                       		 let i = String.index id' '@' in
                        	 let x = int_of_string (String.sub id' (i+1) ((String.length id')-i-1))
                           in id^"@"^(string_of_int (x-1)) 
                          else res
| Seq(c1,c2,_) 			-> searchFirstAssign id c1 (searchFirstAssign id c2 res)
| Nop(_) 				-> res
| Cond(_,c1,c2,_,_) 	-> res
| While(_,c,_,_) 		-> res
;;


let rec genPhiNodes varList c1 c2 = match varList with
  [] -> []
| id::ids -> let id1 = searchLastAssign id c1 (searchFirstAssign id c2 id) in
             let id2 = searchLastAssign id c2 (searchFirstAssign id c1 id) 
             in Phi(ssaIzeRHS id,id1,id2)::(genPhiNodes ids c1 c2) ;;



let rec genWPhiNodes varList c = match varList with
  [] -> []
| id::ids -> (try 
	             let id1 = searchLastAssign id c id in
    	         let id2 = searchFirstAssign id c id in
			 	 let i1 = String.index id1 '@' in
    	         let x1 = int_of_string (String.sub id1 (i1+1) ((String.length id1)-i1-1)) in
			 	 let i2 = String.index id2 '@' in
    	         let x2 = int_of_string (String.sub id2 (i2+1) ((String.length id2)-i2-1)) in
    	         if (((x1=0) && (not (isDef id1 !globalEnv))) || ((x2=0) && (not (isDef id2 !globalEnv)))) then
					genWPhiNodes ids c 
				 else
    	          (Phi(id2,id1,id2))::(genWPhiNodes ids c) 
              with _ -> (genWPhiNodes ids c)
			) ;;


let rec addLoopNodes c varList = match varList with
  [] -> c
| id::ids -> let id1 = searchFirstAssign id c id in
             let id2 = id^"@"^(string_of_int (getEnv id !ssaEnv)) 
             in if ((String.compare id1 id2)=0) then 
                   addLoopNodes c ids
                else                       
                   let c' = Seq(c,Assign(id1,Id(id2,Lab(-23)),Lab(-23)),Lab(-1))
                   in addLoopNodes c' ids ;;



let rec correctExprInElse e varList = match e with
  Id(id,l) -> Id(getEnvComplete (removeSSA id) varList id,l)
| Cst(_) -> e
| Plus(e1,e2,l) -> Plus(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Minus(e1,e2,l) -> Minus(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Times(e1,e2,l) -> Times(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Div(e1,e2,l) -> Div(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Uminus(e1,l) -> Uminus(correctExprInElse e1 varList,l)
| Sqrt(e1,l) -> Sqrt(correctExprInElse e1 varList,l)
| Cos(e1,l) -> Cos(correctExprInElse e1 varList,l)
| Sin(e1,l) -> Sin(correctExprInElse e1 varList,l)
| Exp(e1,l) -> Exp(correctExprInElse e1 varList,l)
| Log(e1,l) -> Log(correctExprInElse e1 varList,l)
| IntOfBool(be,l) -> IntOfBool(correctBExprInElse be varList,l)


and correctBExprInElse be varList = match be with
  BCst(_)      -> be
| Eq(e1,e2,l)  -> Eq(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Lt(e1,e2,l)  -> Lt(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Lte(e1,e2,l) -> Lte(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Gt(e1,e2,l)  -> Gt(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| Gte(e1,e2,l) -> Gte(correctExprInElse e1 varList,correctExprInElse e2 varList,l)
| And(e1,e2,l) -> And(correctBExprInElse e1 varList,correctBExprInElse e2 varList,l)
| Or(e1,e2,l)  -> Or(correctBExprInElse e1 varList,correctBExprInElse e2 varList,l)
| Not(e1,l)    -> Not(correctBExprInElse e1 varList,l)
| BoolOfInt(e1,l) -> BoolOfInt(correctExprInElse e1 varList,l) 
;;


let rec correctPhisInElse phis varList = match phis with
  [] -> ([],varList)
| (Phi(id,id1,id2))::phis' -> 	let id1' = getEnvComplete (removeSSA id1) varList id in
		     		let id2' = getEnvComplete (removeSSA id2) varList id in
                    	 	let id' = getEnvComplete (removeSSA id) varList id in
		   		let varList' = setEnv (removeSSA id) id varList  in
				let (phis'',varList'') = correctPhisInElse phis' varList'
				in ((Phi(id',id1',id2'))::phis'',varList'') ;;





let rec correctElseBranch cElse varList = match cElse with
  Assign(id,e,l) ->     let e' = correctExprInElse e varList in
			let varList' = setEnv (removeSSA id) id varList in
				(Assign(id,e',l),varList')
| Seq(c1,c2,l) -> let (c1',varList') = correctElseBranch c1 varList in
		  let (c2',varList'') = correctElseBranch c2 varList'
		  in (Seq(c1',c2',l),varList'') 
| Nop(_) -> (cElse,varList)
| Cond(be,c1,c2,phis,l) -> 	let be' = correctBExprInElse be varList in
				let (c1',varList') = correctElseBranch c1 varList in
				let (c2',varList'') = correctElseBranch c2 varList' in
		  		let (phis',varList''') = correctPhisInElse phis varList'' 
				in (Cond(be',c1',c2',phis',l),varList''')
| While(be,c1,phis,l) -> 	let be' = correctBExprInElse be varList in
				let (c1',varList') = correctElseBranch c1 varList in
		  		let (phis',varList'') = correctPhisInElse phis varList' 
				in (While(be',c1',phis',l),varList'')
;;
  

let decrementSSAId id =
(*  try *)
    let i = String.index id '@' in
    let v = String.sub id (i+1) ((String.length id)-i-1) in   
    let x = max 0 ((int_of_string v) - 1)
    in (removeSSA id)^"@"^(string_of_int x) 
(*  with _ -> id  *)
;;


let rec buildPhisVarList phis varList = match phis with
  [] -> varList
| Phi(id,_,_)::phis' -> if (isDef id varList) then varList
		    	else 
				let id' = decrementSSAId id in 
				let varList' = setEnv (removeSSA id) id' varList
				in buildPhisVarList phis' varList'
;;


let rec buildElseVarList cThen varList = match cThen with
  Assign(id,_,_) -> if (isDef id varList) then varList
		    else 
			let id' = decrementSSAId id 
			in setEnv (removeSSA id) id' varList
| Seq(c1,c2,l) -> let varList' = buildElseVarList c1 varList 
		  in buildElseVarList c2 varList'
| Nop(_) -> varList
| Cond(be,c1,c2,phis,l) -> buildPhisVarList phis varList
| While(be,c1,phis,l) -> buildPhisVarList phis varList
;;


let genCond be c1 c2 lab =
  let varList = buildElseVarList c1 [] in
  let (c2',_) = correctElseBranch c2 varList in
  let varList = joinLists (getAssignedVars c1) (getAssignedVars c2') in
  let phiNodeList = genPhiNodes varList c1 c2' in
     Cond(be,c1,c2',phiNodeList,lab) ;;


let genWhile be c lab =
  let varList = getAssignedVars c in
  let phiNodeList = genWPhiNodes varList c in
        While(be,c,phiNodeList,lab) ;;


let rec translateThenPhis c phis = match phis with
  [] -> c
| (Phi(id,id1,id2))::phis' -> translateThenPhis (Seq(c,Assign(id,Id(id1,Lab(0)),Lab(0)),Lab(0))) phis' ;;



let rec translateElsePhis c phis = match phis with
  [] -> c
| (Phi(id,id1,id2))::phis' -> translateElsePhis (Seq(c,Assign(id,Id(id2,Lab(0)),Lab(0)),Lab(0))) phis' ;;



let rec getWPhiBlackList phis = match phis with
  [] -> []
| Phi(y,x,_)::phis' -> y::x::(getWPhiBlackList phis') ;;









