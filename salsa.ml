
open Prelude ;;
open Parser;;
open Print ;;
open Ssa ;;


(*
			let _ = (print_string (green^"\n  ~o__.__O  "^cc^"+---------------------------------------------------------------------------+\n")  ;
                                 print_string (green^"\\_)--:--/          "^cyan^"Welcome to  E X P R E S S I O N   S A L S A  Version 0.1\n")) in
			let _ = print_string (green^" /     <\\  "^cc^"+---------------------------------------------------------------------------+"^cc) in 
	let _ = flush stdout in
*)
let main () = 
    let _ = print_string (black^"\n+---------------------------------------------------------------------------------------------------------------------------------------------+") in
    let _ = print_string ("\n|"^magenta^" ~o__.__O                                                                                                                         ~o__.__O"^black^" |") in
    let _ = print_string ("\n|"^magenta^"\\_)--:--/                                  Welcome to   C O M P U T A T I O N   S A L S A   Version 0.9.                         \\_)--:--/ "^black^"|") in
    let _ = print_string ("\n|"^magenta^" /     <\\                                                                                                                         /     <\\"^black^" |") in
    let _ = print_string ("\n+---------------------------------------------------------------------------------------------------------------------------------------------+\n") in
	let name = (Array.get Sys.argv 1) in
        let _ = print_string ("Starting transformation \n") in let _ = flush stdout in
	let _ = Sys.command ("./salsa-rw-opt "^name) in
	let f = open_in (name^".tmp") in
	let lexbuf = Lexing.from_channel f in 
	(*let (ast,varRef,initEnv) = program Lexer.token lexbuf in (* r contient le resultat du parseur cad l'arbre syntaxique du prog *)
 	let _ = close_in f in
	let v = removeSSA varRef in
    let _ = print_string (blue^(printCommand (removeSSACmd ast) 0 )) in
    let _ = flush stdout in *) 
	
	let _ = Sys.command ("./salsa-eval-opt "^name^" 0") in
	let _ = Sys.command ("./salsa-eval-opt "^name^".tmp 1") in 
(*	let _ = Sys.command ("./salsa-meas-opt "^name) in   *)
        let ss = string_of_int !sliceSize in
 (*    let _ = print_string ("| Program:"^blue^name^(String.make (70-(String.length name)) ' ')^black^"|"^blue^"Identifier: "^v^(String.make (27-(String.length v)) ' ')^black^"| Slice Size:"^magenta^ss^(String.make (3-(String.length ss)) ' ')^black^"|") in *) 
        let _ = print_string ("\n+---------------------------------------------------------------------------------+-----------------------------------------+-----------------+\n") in
		(print_string "\n\n")
;;

(* 
let name = (Array.get Sys.argv 1) in
	let f = open_in (name) in
	let lexbuf = Lexing.from_channel f in 
	let (ast,varRef,initEnv) = program Lexer.token lexbuf in (* r contient le resultat du parseur cad l'arbre syntaxique du prog *)
 	let _ = close_in f in
	let v = removeSSA varRef in
        let _ = print_string (blue^(printCommand (removeSSACmd ast) 0 )) in ();;

*)
		  
main () ;;	
 
 
