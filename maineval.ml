
open SalsaTypes ;;
open Epeg_types ;;
open Parser ;;
open Prelude ;;
open Ssa ;;
open Rewrite ;;
open Float;;
open Print ;;
open Num ;;
open Measure ;;
open Function ;;

let init () = 
  	(ssaEnv := [] ;
	 codeLine := 1 ;
    );;



let printCstBis (u,v) = 
  let f = printFInterval u in
  let e = printRInterval v in
  let f' = f^(String.make (max 1 (52-(String.length f))) ' ') in
  let e' = e^(String.make (max 1 (52-(String.length e))) ' ') in
	black^"FP Val:"^green^f'^black^"| Err:"^yellow^e' ;;


let evalSrc ast varRef vInit initEnv = 
         let v = (match vInit with
                    (I(_),J(a,b)) -> max (abs_float a) (abs_float b) 
                  | _ -> max_float
                 )
             in    
              (
               print_string (black^"\n+------------+--------------------------------------------------------------------+-----------------------------------------------------------+")  ;
               print_string (black^"\n|"^magenta^" ~o__.__O "^black^"| Src |"^(printCstBis vInit)^black^"|") ;
   			   let f = open_out ((Array.get Sys.argv 1)^".tmp.ev") in
               let _ = output_string f ((string_of_float v)^"\n") in
                 close_out f
             ) ;;
 

let evalOpt ast varRef vInit initEnv = 
         let vo = (match vInit with
                    (I(_),J(a,b)) -> max (abs_float a) (abs_float b) 
                  | _ -> max_float
                 )
         in
         let f = open_in ((Array.get Sys.argv 1)^".ev") in
         let vs = (float_of_string (input_line f)) in
         let _ = close_in f in
         let p = if ((vs=max_float) || (vo=max_float)) then "???" else (string_of_float ((abs_float (vs-.vo))*. 100. /. vs)) in
         let i = if ((vs=max_float) || (vo=max_float)) then "???" else (string_of_float (abs_float (vs-.vo))) in
         let col = if (vo < vs) then green else yellow in 
               print_string (black^"\n|"^magenta^"\\_)--:--/"^black^" | Opt |"^(printCstBis vInit)^black^"|") ;
               print_string (black^"\n|"^magenta^" /     <\\ "^black^"+-----------+--------------------------------------------------------+-----------------------------------------------------------+") ;
               print_string ("\n|"^magenta^"S A L S A"^black^" | Improvement (value):"^col^i^(String.make (45-(String.length i)) ' ')^black^"| Improvement (%):"^col^p^" %"^(String.make (38-(String.length p)) ' ')^black^"|") ;
               print_string (black^"\n+------------+--------------------------------------------------------------------+-----------------------------------------+-----------------+\n")   
               ;;

let main () = 
		 (try  
			let f = open_in (Array.get Sys.argv 1) in
			let lexbuf = Lexing.from_channel f in 
			let (initEnv) = program Lexer.token lexbuf in 
 			let _ = close_in f in
                        let _ = codeLine := -1 in
			let _ = globalEnv := !initEnv in
			let _ = dynVars := List.map fst !initEnv in
			let f = getFunc "main" !funList in 
         		let vInit = evalFunc f [] !initEnv in
			print_string ("eval result: "^ (printCst vInit)^"\n")
			(*
                        let p = (try (if ((String.compare (Sys.argv.(2)) "0")=0) then false else true) with _ -> false) in
                           if (not p) then evalSrc ast varRef vInit initEnv else evalOpt ast varRef vInit initEnv
		           with 		
		             Error(e) -> if (!codeLine >=0) then  
                                print_string (cc^"\nParse error at line "^(string_of_int (!codeLine))^" \n")
                           else
                                print_string (cc^"\n"^e^" \n")     *)
                  with 
		| EPEGError(e1,e2) -> print_string (cc^"\nEPEG Error "^e1^": "^e2^" \n")
                | Parsing.Parse_error -> print_string (cc^"\nParse error at line "^(string_of_int (!codeLine))^" \n")
         (*       | _ -> print_string (cc^"\nUnknown error at line "^(string_of_int (!codeLine))^" \n")     
	*)	) ; 
;;
		
		  
main () ;;	
 
 
 
 
