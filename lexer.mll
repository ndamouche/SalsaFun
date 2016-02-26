

{

open SalsaTypes;;
open Parser ;;
open Prelude ;;

}

rule token = parse
  ' '										{ token lexbuf }
| '\t'                                                      	        	{ token lexbuf }
| '\n'                                						{ codeLine := !codeLine+1; token lexbuf }
| '\r'										{ token lexbuf }
| eof                                                        			{ EOF }
| ('-'?)(['0'-'9']*)('.')(['0'-'9']*)((('e'|'E')(('-'?)('+'?)(['0'-'9'])+))?)  	{   let s = Lexing.lexeme lexbuf 
											in FLOAT(float_of_string s)
	 									}				
| ('-'?)(['0'-'9']+)(('e'|'E')(('-'?)('+'?)(['0'-'9'])+))	  		{   let s = Lexing.lexeme lexbuf 
									         	in FLOAT(float_of_string s)
		 								}	
| '{'                                                              		{ ACCBEG }
| '}'                                                              		{ ACCEND }
| '('                                                              		{ PARBEG }
| ')'                                                              		{ PAREND }
| '['                                                              		{ BRABEG }
| ']'                                                              		{ BRAEND }
| '*'			                                                        { MULT }
| '/'			                                                        { DIV }
| "%salsa%"			                                                { STOP }
| '+'                                                              		{ PLUS }
| '-'                                                              		{ MINUS }
| "<=" 										{ LTE }
| '<'                                                              		{ LT }
| ">=" 										{ GTE }
| '>'                                                              		{ GT }
| '&'                                                              		{ AND }
| '|'                                                              		{ OR }
| '!'                                                              		{ NOT }
| ';'                                                              		{ SC }
| "while"									{ WHILE }
| "for"										{ FOR }
| "if"										{ IF }
| "else"									{ ELSE }
| "true"									{ TRUE }
| "false"									{ FALSE }
| "nop"					   					{ NOP }
| "sqrt"									{ SQRT }
| "cos"										{ COS }
| "sin"										{ SIN }
| "exp"										{ EXP }
| "log"										{ LOG }
| "&&" 								        	{ AND }
| "||" 										{ OR }
| "!" 									        { NOT }
| "intOfBool"									{ INTOFBOOL } 
| "boolOfInt" 									{ BOOLOFINT }
| ','                                                              		{ COMMA }
| '='                                                              		{ EGAL }
| "double"                                                                      { DOUBLE }
| "float"                                                                       { DOUBLE }
| "return"                                                                      { RETURN } 
| (['a'-'z']|['A'-'Z']|'_')((['a'-'z']|['A'-'Z']|'_'|['0'-'9'])*) as word       { ID(word) } 
