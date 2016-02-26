
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

let init () = 
  	(ssaEnv := [] ;
	 codeLine := 1 ;
    );;


let rec iterRewrite cList = 
	     let _ = print_string "." in let _ = flush stdout in
	     let _ = init () in
       	     let f = open_in ((Array.get Sys.argv 1)^".tmp") in
	     let lexbuf = Lexing.from_channel f in 
	     let (initEnv) = program Lexer.token lexbuf in
	  (* let (ast,varRef,initEnv) = program Lexer.token lexbuf in *) 
             let _ = close_in f in
             let _ = codeLine := -1 in
 	     let _ = globalEnv := !initEnv in
       	     let _ = dynVars := List.map fst !initEnv in
       	     
       	     let Func(name, args, ast, varRef, lab) = getFunc "main" !funList in 
	     let env' = evalCommand ast !initEnv in
	                     
       	     let (bestCmd',bestEnv') = rewriteCommand ast [] Hole varRef [] in
             let (bestCmd'',_) = try cleanTmpVars bestCmd' [] varRef [] with _ -> (bestCmd',[]) in 
	     let (bestCmd''',_,_) = try evalPartCmd bestCmd'' (valEnv2ExprEnv !initEnv) [] with _ -> (bestCmd'',[],[]) in
             let  bestCmd'''' = try removeUnreadVars bestCmd''' (varRef::(readVarsCmd bestCmd''')) with _ -> bestCmd''' in
            		if (not (memberCmd (removeSSACmd bestCmd'''') cList)) then 
                        let cmdStr = filePrintCommand (removeSSACmd bestCmd'''') varRef !initEnv true in
			let f = open_out ((Array.get Sys.argv 1)^".tmp") in
			let _ = output_string f cmdStr in
			let _ = close_out f in
			        iterRewrite ((removeSSACmd bestCmd'''')::cList) 
               		else    ();;
		
		  
 
let main () = 
		 (try  
			let f = open_in (Array.get Sys.argv 1) in
			let lexbuf = Lexing.from_channel f in 
			let (initEnv) = program Lexer.token lexbuf in  (* r contient le resultat du parseur cad l'arbre syntaxique du prog *)
 			let _ = close_in f in
                        let _ = codeLine := -1 in
			let _ = globalEnv := !initEnv in
			let _ = dynVars := List.map fst !initEnv in
			
			let f = getFunc "main" !funList in 
			let _ = transformFunction f in 
	         (*       let env' = evalCommand ast !initEnv in *) 
       	                
			let cmdStr = filePrintFuncList (!funListTransform) in
			let start = (filePrintEnv !initEnv)^"\n%salsa%\n\n" in 
			let f = open_out ((Array.get Sys.argv 1)^".tmp") in
			let _ = output_string f (start^cmdStr) in
			let _ = close_out f in 
                 (*       let _ = iterRewrite [ast]  in () *) 
                        () 
		 with 
		
		
		Error(e) -> if (!codeLine >=0) then  
                                print_string (cc^"\nParse error at line "^(string_of_int (!codeLine))^" \n")
                            else
                                print_string (cc^"\n"^e^" \n")  
		| EPEGError(e1,e2) -> print_string (cc^"\nEPEG Error "^e1^": "^e2^" \n")
                | Parsing.Parse_error -> print_string (cc^"\nParse error at line "^(string_of_int (!codeLine))^" \n")
    	) ; 
;;
		
		  
main () ;;	
 
 
 
 
