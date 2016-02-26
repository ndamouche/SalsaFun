type token =
  | DOUBLE
  | RETURN
  | PLUS
  | MINUS
  | MULT
  | DIV
  | EGAL
  | COMMA
  | WHILE
  | FOR
  | IF
  | ELSE
  | FALSE
  | TRUE
  | SC
  | PARBEG
  | PAREND
  | ACCBEG
  | ACCEND
  | BRABEG
  | BRAEND
  | OPTIMIZE
  | STOP
  | EOF
  | LT
  | LTE
  | GT
  | GTE
  | AND
  | OR
  | NOT
  | NOP
  | SQRT
  | COS
  | SIN
  | EXP
  | LOG
  | INTOFBOOL
  | BOOLOFINT
  | ID of (string)
  | FLOAT of (float)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  (((string * SalsaTypes.abstractValue) list) ref) 
