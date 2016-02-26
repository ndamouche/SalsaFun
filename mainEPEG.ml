
open SalsaTypes ;;
open Prelude ;;
open Float ;;
open Epeg_types ;;
open Ast2peg ;;
open Peg2epeg ;;
open Epeg2epegsharp ;;
open Profitability ;;


(* used to apply the abstraction algorithm : Horizontal *)
let use_horizontal_abstraction = ref true ;;
(* used to apply the abstraction algorithm : Vertical *)
let use_vertical_abstraction = ref true ;;
(* used to apply the abstraction algorithm : Expansion *)
let use_boxexpansion_abstraction = ref true ;;
(* used to apply the abstraction algorithm : SumCombinaison *)
let use_sumcombinaison_abstraction = ref true ;;


(* enlever la fonction *)
let pp_val fmt (f,r) =
let module ST = SalsaTypes in 
match f,r with
| ST.I(a,b), ST.J(c,d) -> Format.fprintf fmt "[%f, %f] + [%e, %e]" a b c d
| ST.I(a,b), ST.JInfty -> Format.fprintf fmt "[%f, %f] + oo" a b
| _ -> Format.fprintf fmt "???"

  let transformExpression id e env =
  let aff = Affectation(id,e) in
  let pegs = [convertASTtoSPEG aff] in
(*  let initProfitabilityResult = analyzeProgram pegs profitabilityEnv in *)
  let _ = doEPEGConstructionOn pegs in
  let _ = if (!use_horizontal_abstraction) then List.map doHorizontalAbstraction pegs else [] in
  let _ = if (!use_vertical_abstraction) then List.map doVerticalAbstraction pegs else [] in
  let _ = if (!use_boxexpansion_abstraction) then List.map doBoxExpansionAbstraction pegs else [] in
  let _ = if (!use_sumcombinaison_abstraction) then List.map doSumCombinaisonAbstraction pegs else [] in
  let profitabilityResult = analyzeProgram pegs env in 
  let (resultExpr,resultVal) = getEnv id profitabilityResult in
    (resultExpr,resultVal) ;;



