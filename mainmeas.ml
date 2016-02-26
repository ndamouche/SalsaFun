
open SalsaTypes ;;
open Epeg_types ;;
open Parser ;;
open Prelude ;;
open Ssa ;;
open Print ;;
open Rewrite ;;
open Float;;
open Num ;;
open Measure ;;
open Function ;; 


let main () = 
			let f = open_in (Array.get Sys.argv 1) in
			let lexbuf = Lexing.from_channel f in 
			let (initEnv) = program Lexer.token lexbuf in (* r contient le resultat du parseur cad l'arbre syntaxique du prog *)
 			let _ = close_in f in
			let f' = open_in ((Array.get Sys.argv 1)^".tmp") in
			let lexbuf = Lexing.from_channel f' in 
			let (initEnv') = program Lexer.token lexbuf in (* r contient le resultat du parseur cad l'arbre syntaxique du prog *)
 			let _ = close_in f' in
                        let _ = codeLine := -1 in
			let _ = globalEnv := !initEnv in
			let _ = dynVars := List.map fst !initEnv in
			
                        let Func(name, args, ast, varRef, lab) = getFunc "main" !funList in 
			let env' = evalCommand ast !initEnv in
			
                        let _ = measureF ast (Array.get Sys.argv 1) (removeSSA varRef) in   (* Programme Flottant initial *)
                        let _ = measureQ ast (Array.get Sys.argv 1) (removeSSA varRef) in   (* Programme Q initial *) 
                        let _ = measureF (removeSSACmd ast') ((Array.get Sys.argv 1)^"_t") (removeSSA varRef') in (* Programme Flottant Transformé  *)
                        let _ = measureQ (removeSSACmd ast') ((Array.get Sys.argv 1)^"_t") (removeSSA varRef') in (* Programme Flottant Transformé  *)
			let _ = measErr () in
			let _ = createGPErr (Array.get Sys.argv 1) in 
			let _ = createGP (Array.get Sys.argv 1) in
               ()
;;
		
		  
main () ;;	
 
 
 
 
