%{

open SalsaTypes ;;
open Prelude ;;
open Ssa ;;
open Float ;;

%}

%type < (((string * SalsaTypes.abstractValue) list) ref) > program
%start program


%token DOUBLE RETURN 
%token PLUS MINUS MULT DIV EGAL COMMA WHILE FOR IF ELSE FALSE TRUE 
%token SC PARBEG PAREND ACCBEG ACCEND BRABEG BRAEND OPTIMIZE STOP
%token EOF LT LTE GT GTE AND OR NOT NOP SQRT COS SIN EXP LOG INTOFBOOL BOOLOFINT 
%token <string> ID 
%token <float> FLOAT
%left PLUS MINUS
%left MULT 

%%
program			: header prog EOF					{ let _ = funList := $2 in $1 }				
;
header                  : STOP  				        	{ ref [] }
                        | declList STOP                  	 		{ ref $1 }
;
declList                : ID EGAL cst                       			{ (ssaIzeRHS $1,$3)::[] }
                        | ID EGAL cst declList             			{ (ssaIzeRHS $1,$3)::$4 }
;
cmd 		        : ID EGAL expr						{ Assign(ssaIzeLHS $1,$3,Lab(!codeLine)) }
			| NOP							{ Nop(Lab(!codeLine)) }
			| cmd SC cmd						{ Seq($1,$3,Lab(!codeLine)) }
			| IF bexpr ACCBEG cmd ACCEND ELSE ACCBEG cmd ACCEND     { genCond $2 $4 $8 (Lab(!codeLine)) }
			| IF bexpr ACCBEG cmd ACCEND                      	{ genCond $2 $4 (Nop(Lab(!codeLine))) (Lab(!codeLine)) }
			| WHILE bexpr ACCBEG cmd ACCEND   	        	{ try genWhile $2 $4 (Lab(!codeLine)) with _ -> raise (Error "ntnt")}
			| ACCBEG cmd ACCEND					{ $2 }
                        | cmd SC				                { $1 }
;
expr		        : cst					  		{ Cst($1,Lab(!codeLine)) }
   			| ID                                                    { Id(ssaIzeRHS $1,Lab(!codeLine)) }
			| expr PLUS expr           	            	        { Plus($1,$3,Lab(!codeLine)) }
			| expr MINUS expr  	       			        { Minus($1,$3,Lab(!codeLine)) }
			| expr MULT expr  	       			        { Times($1,$3,Lab(!codeLine)) }
			| expr DIV expr  	       			        { Div($1,$3,Lab(!codeLine)) }
			| MINUS expr					        { Uminus($2,Lab(!codeLine)) }    
			| SQRT PARBEG expr PAREND				{ Sqrt($3,Lab(!codeLine)) }     
			| COS PARBEG expr PAREND				{ Cos($3,Lab(!codeLine)) }     
			| SIN PARBEG expr PAREND				{ Sin($3,Lab(!codeLine)) }     
			| EXP PARBEG expr PAREND				{ Exp($3,Lab(!codeLine)) }     
			| LOG PARBEG expr PAREND				{ Log($3,Lab(!codeLine)) } 
			| INTOFBOOL PARBEG bexpr PAREND				{ IntOfBool($3,Lab(!codeLine)) }    
			| ID PARBEG exprList PAREND                             { FunCall($1, $3, Lab(!codeLine)) }
			| PARBEG expr PAREND				        { $2 }
;
bexpr	  	        : TRUE					    	        { BCst(true,Lab(!codeLine)) }
   			| FALSE                                                 { BCst(false,Lab(!codeLine)) }
			| expr LT expr  	       			        { Lt($1,$3,Lab(!codeLine)) }
			| expr LTE expr  	       			        { Lte($1,$3,Lab(!codeLine)) }
			| expr GT expr  	       			        { Gt($1,$3,Lab(!codeLine)) }
			| expr GTE expr  	       			        { Gte($1,$3,Lab(!codeLine)) }
			| expr EGAL EGAL expr  			   	        { Eq($1,$4,Lab(!codeLine)) }
			| bexpr AND bexpr  	       			        { And($1,$3,Lab(!codeLine)) }
			| bexpr OR bexpr  	       			        { Or($1,$3,Lab(!codeLine)) }
			| NOT bexpr  	       			                { Not($2,Lab(!codeLine)) }
			| PARBEG bexpr PAREND				        { $2 }
			| BOOLOFINT PARBEG expr PAREND				{ BoolOfInt($3,Lab(!codeLine)) }
;
cst			: signedFloat						{ (I($1,$1),ulp(I($1,$1)))  }
			| BRABEG signedFloat COMMA signedFloat BRAEND           { (I($2,$4),ulp(I($2,$4))) }
			| BRABEG signedFloat COMMA signedFloat COMMA signedFloat COMMA signedFloat BRAEND 											{ (I($2,$4),r_of_f (I($6,$8))) }
;
signedFloat		: FLOAT							{ $1 }
;
func                    : DOUBLE ID PARBEG paramList PAREND ACCBEG cmd RETURN ID SC ACCEND   
                                                                                { Func($2, $4, $7, ssaIzeRHS $9, Lab(!codeLine)) }
                        | DOUBLE ID PARBEG PAREND ACCBEG cmd RETURN ID SC ACCEND   
                                                                                { Func($2,[] , $6, ssaIzeRHS $8, Lab(!codeLine)) }
;
exprList                : expr                                                  { $1::[] }
                        | expr COMMA exprList                                   { $1::$3 } 
;
paramList               : DOUBLE ID                                             { (ssaIzeRHS $2)::[] }
                        | DOUBLE ID COMMA paramList                             { (ssaIzeRHS $2)::$4 } 
;
prog                    : func                                                  { $1::[] }
                        | func prog                                             { $1::$2 }
; 
%%
