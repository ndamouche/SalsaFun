open SalsaTypes ;;
open Prelude ;;
open Rewrite ;;






  

let rec buildArgsValues args el idf l = match args with 
   [] -> (match el with 
           [] -> Nop(Lab(0)) 
         | _  -> raise (Error ("Too Many Arguments in Call to "^idf))
         )
|  x::xs -> (match el with 
              []    ->  raise (Error ("Too Few Arguments in Call to "^idf))
            | e::es ->  Seq(Assign(x,e,l),buildArgsValues xs es idf l, l)
            )
;;


let ruleSelector idf = Value ;;


let inlineFun name args return lab idf el id l2 = 
  let Func(_, argsf, cf, ret, lf)  = getFunc idf !funList in 
  let assignList   = buildArgsValues argsf el idf lf in 
  let finalAssign  = Assign(id, Id(ret, lf), l2) in
  Seq(assignList, cf, lab)
  ;;


let valueFun name args return lab idf el id l2 = 
  let Func(_, argsf, cf, ret, lf)  = getFunc idf !funList in 
  let assignList   = buildArgsValues argsf el idf lf in 
  let c = Seq(assignList, cf, lab) in 
  let (c',_) = rewriteCommand c [] Hole ret [] in 
  let idf' = idf^(newVar()) in 
  let g = Func(idf', argsf, c', ret, lf) in 
  let _ = funListTransform := g::(!funListTransform) in 
  let _ = funList := g::(!funList) in 
   Assign(id, FunCall(idf', el, lf),lf)
;;
  

let rec transformFunCmd name args c return lab ruleSelector = match c with 
  Assign(id, FunCall(idf, el, l1),l2) -> (match ruleSelector idf with 
                                         Inline -> inlineFun name args return lab idf el id l2    
                                       | Value  -> valueFun name args return lab idf el id l2
                                      (* | Slice  -> sliceFun name args c return lab idf 
                                       | Formal -> formalFun name args c return lab idf  *)
                                        )
| Assign(_)                           -> c   
| Nop(_)                              -> c 
| Seq(c1, c2, l)                      -> let c1' = transformFunCmd name args c1 return l ruleSelector in 
                                         let c2' = transformFunCmd name args c2 return l ruleSelector in
                                           Seq(c1', c2', l)
| Cond(be, c1, c2, phi, l)            -> let c1' = transformFunCmd name args c1 return l ruleSelector in
                                         let c2' = transformFunCmd name args c2 return l ruleSelector in 
                                           Cond(be, c1', c2', phi, l)  
| While(be, c, phi, l)                -> let c' = transformFunCmd name args c return l ruleSelector in
                                           While(be, c', phi, l)  
;;


let transformFunction f  =  match f with 
Func(name, args, c, return, lab) -> let c' = transformFunCmd name args c return lab ruleSelector in 
                                    let (c'',_) = rewriteCommand c' [] Hole return []   in 
                                        funListTransform := (Func(name, args, c'', return, lab))::(!funListTransform) ;;
 
    
