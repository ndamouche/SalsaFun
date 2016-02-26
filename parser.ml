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

open Parsing;;
let _ = parse_error;;
# 1 "./parser.mly"


open SalsaTypes ;;
open Prelude ;;
open Ssa ;;
open Float ;;

# 55 "./parser.ml"
let yytransl_const = [|
  257 (* DOUBLE *);
  258 (* RETURN *);
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* MULT *);
  262 (* DIV *);
  263 (* EGAL *);
  264 (* COMMA *);
  265 (* WHILE *);
  266 (* FOR *);
  267 (* IF *);
  268 (* ELSE *);
  269 (* FALSE *);
  270 (* TRUE *);
  271 (* SC *);
  272 (* PARBEG *);
  273 (* PAREND *);
  274 (* ACCBEG *);
  275 (* ACCEND *);
  276 (* BRABEG *);
  277 (* BRAEND *);
  278 (* OPTIMIZE *);
  279 (* STOP *);
    0 (* EOF *);
  280 (* LT *);
  281 (* LTE *);
  282 (* GT *);
  283 (* GTE *);
  284 (* AND *);
  285 (* OR *);
  286 (* NOT *);
  287 (* NOP *);
  288 (* SQRT *);
  289 (* COS *);
  290 (* SIN *);
  291 (* EXP *);
  292 (* LOG *);
  293 (* INTOFBOOL *);
  294 (* BOOLOFINT *);
    0|]

let yytransl_block = [|
  295 (* ID *);
  296 (* FLOAT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\005\000\005\000\005\000\010\000\011\000\011\000\009\000\009\000\
\012\000\012\000\003\000\003\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\003\000\004\000\003\000\001\000\003\000\
\009\000\005\000\005\000\003\000\002\000\001\000\001\000\003\000\
\003\000\003\000\003\000\002\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\003\000\001\000\001\000\003\000\003\000\
\003\000\003\000\004\000\003\000\003\000\002\000\003\000\004\000\
\001\000\005\000\009\000\001\000\011\000\010\000\001\000\003\000\
\002\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\053\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\044\000\000\000\041\000\
\000\000\001\000\052\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\042\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\050\000\000\000\030\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\043\000\021\000\022\000\023\000\
\024\000\025\000\026\000\040\000\000\000\027\000\000\000\011\000\
\000\000\046\000\000\000\048\000\000\000\045\000\000\000\000\000\
\009\000"

let yydgoto = "\002\000\
\005\000\006\000\010\000\007\000\056\000\039\000\057\000\058\000\
\109\000\016\000\011\000\026\000"

let yysindex = "\014\000\
\246\254\000\000\000\000\035\255\000\000\066\255\068\255\239\254\
\079\255\079\000\066\255\000\000\082\255\000\000\111\255\000\000\
\179\255\000\000\000\000\170\255\000\000\103\255\082\255\145\255\
\178\255\186\255\000\255\198\255\017\255\192\255\082\255\000\000\
\203\255\050\255\050\255\017\255\000\000\204\255\007\255\017\255\
\216\255\000\000\061\255\000\000\000\000\050\255\050\255\207\255\
\211\255\250\255\251\255\041\000\042\000\045\000\046\000\000\000\
\225\255\031\000\038\000\251\254\061\255\173\255\017\255\047\255\
\082\255\061\255\045\255\252\255\040\255\119\255\061\255\061\255\
\061\255\061\255\061\255\050\255\061\255\061\255\061\255\061\255\
\061\255\061\255\253\255\061\255\061\255\061\255\061\255\017\255\
\050\255\050\255\017\255\000\000\235\255\219\255\247\255\229\255\
\020\000\123\255\000\000\000\000\140\255\168\255\196\255\021\000\
\025\000\139\255\029\000\065\000\028\000\045\255\045\255\059\000\
\235\255\061\255\235\255\235\255\235\255\235\255\253\254\119\255\
\119\255\059\255\053\000\060\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\255\000\000\235\255\000\000\
\062\000\000\000\057\000\000\000\061\000\000\000\017\255\084\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\077\001\000\000\000\000\000\000\055\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\063\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\106\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\255\000\000\
\000\000\000\000\162\255\000\000\000\000\137\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\255\000\000\005\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\064\000\000\000\190\255\218\255\134\255\
\246\255\000\000\088\255\019\000\022\000\026\000\000\000\158\255\
\165\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\000\000\000\
\100\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\071\001\068\001\076\001\220\255\215\255\227\255\
\208\000\244\255\000\000\053\001"

let yytablesize = 342
let yytable = "\060\000\
\020\000\067\000\013\000\064\000\068\000\059\000\008\000\031\000\
\062\000\063\000\027\000\063\000\003\000\092\000\001\000\136\000\
\069\000\070\000\041\000\093\000\032\000\063\000\014\000\008\000\
\098\000\034\000\095\000\035\000\004\000\101\000\102\000\103\000\
\104\000\105\000\036\000\107\000\108\000\110\000\111\000\112\000\
\113\000\008\000\115\000\116\000\117\000\118\000\106\000\037\000\
\096\000\081\000\082\000\119\000\097\000\043\000\122\000\038\000\
\100\000\013\000\006\000\120\000\121\000\063\000\044\000\045\000\
\043\000\046\000\009\000\089\000\090\000\013\000\013\000\006\000\
\135\000\063\000\013\000\006\000\066\000\137\000\018\000\047\000\
\013\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\014\000\012\000\108\000\048\000\049\000\050\000\051\000\
\052\000\053\000\063\000\055\000\014\000\010\000\145\000\024\000\
\031\000\031\000\144\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\010\000\031\000\031\000\017\000\010\000\025\000\
\015\000\014\000\015\000\015\000\015\000\079\000\080\000\081\000\
\082\000\015\000\015\000\015\000\015\000\015\000\015\000\018\000\
\018\000\018\000\018\000\099\000\018\000\018\000\079\000\080\000\
\081\000\082\000\089\000\090\000\018\000\004\000\018\000\018\000\
\018\000\038\000\038\000\131\000\126\000\018\000\018\000\018\000\
\018\000\018\000\018\000\020\000\020\000\020\000\089\000\090\000\
\020\000\020\000\079\000\080\000\081\000\082\000\036\000\036\000\
\020\000\023\000\020\000\020\000\020\000\037\000\037\000\028\000\
\127\000\020\000\020\000\020\000\020\000\020\000\020\000\016\000\
\016\000\016\000\022\000\029\000\016\000\016\000\079\000\080\000\
\081\000\082\000\030\000\024\000\016\000\033\000\016\000\016\000\
\016\000\040\000\061\000\094\000\128\000\016\000\016\000\016\000\
\016\000\016\000\016\000\017\000\017\000\017\000\071\000\065\000\
\017\000\017\000\072\000\079\000\080\000\081\000\082\000\083\000\
\017\000\123\000\017\000\017\000\017\000\079\000\080\000\081\000\
\082\000\017\000\017\000\017\000\017\000\017\000\017\000\019\000\
\084\000\085\000\086\000\087\000\019\000\019\000\079\000\080\000\
\081\000\082\000\083\000\114\000\019\000\063\000\019\000\019\000\
\019\000\073\000\074\000\124\000\099\000\019\000\019\000\019\000\
\019\000\019\000\019\000\084\000\085\000\086\000\087\000\079\000\
\080\000\081\000\082\000\079\000\080\000\081\000\082\000\079\000\
\080\000\081\000\082\000\032\000\032\000\129\000\033\000\033\000\
\125\000\130\000\034\000\034\000\134\000\132\000\032\000\032\000\
\088\000\033\000\033\000\035\000\035\000\034\000\034\000\091\000\
\075\000\076\000\089\000\090\000\077\000\078\000\035\000\035\000\
\082\000\089\000\090\000\079\000\080\000\081\000\082\000\138\000\
\133\000\141\000\139\000\142\000\051\000\004\000\143\000\049\000\
\047\000\019\000\021\000\015\000\140\000\042\000"

let yycheck = "\036\000\
\013\000\043\000\020\001\040\000\046\000\035\000\002\001\008\001\
\002\001\015\001\023\000\015\001\023\001\019\001\001\000\019\001\
\046\000\047\000\031\000\061\000\021\001\015\001\040\001\019\001\
\066\000\009\001\063\000\011\001\039\001\071\000\072\000\073\000\
\074\000\075\000\018\001\077\000\078\000\079\000\080\000\081\000\
\082\000\007\001\084\000\085\000\086\000\087\000\076\000\031\001\
\002\001\005\001\006\001\088\000\065\000\004\001\091\000\039\001\
\017\001\002\001\002\001\089\000\090\000\015\001\013\001\014\001\
\004\001\016\001\001\001\028\001\029\001\020\001\015\001\015\001\
\114\000\015\001\019\001\019\001\016\001\019\001\000\000\030\001\
\020\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\023\001\133\000\032\001\033\001\034\001\035\001\
\036\001\037\001\015\001\039\001\040\001\002\001\019\001\001\001\
\017\001\018\001\143\000\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\015\001\028\001\029\001\039\001\019\001\017\001\
\015\001\040\001\017\001\018\001\019\001\003\001\004\001\005\001\
\006\001\024\001\025\001\026\001\027\001\028\001\029\001\002\001\
\003\001\004\001\005\001\017\001\007\001\008\001\003\001\004\001\
\005\001\006\001\028\001\029\001\015\001\039\001\017\001\018\001\
\019\001\017\001\018\001\017\001\017\001\024\001\025\001\026\001\
\027\001\028\001\029\001\002\001\003\001\004\001\028\001\029\001\
\007\001\008\001\003\001\004\001\005\001\006\001\017\001\018\001\
\015\001\008\001\017\001\018\001\019\001\017\001\018\001\039\001\
\017\001\024\001\025\001\026\001\027\001\028\001\029\001\002\001\
\003\001\004\001\016\001\018\001\007\001\008\001\003\001\004\001\
\005\001\006\001\017\001\001\001\015\001\008\001\017\001\018\001\
\019\001\018\001\007\001\039\001\017\001\024\001\025\001\026\001\
\027\001\028\001\029\001\002\001\003\001\004\001\016\001\008\001\
\007\001\008\001\016\001\003\001\004\001\005\001\006\001\007\001\
\015\001\015\001\017\001\018\001\019\001\003\001\004\001\005\001\
\006\001\024\001\025\001\026\001\027\001\028\001\029\001\002\001\
\024\001\025\001\026\001\027\001\007\001\008\001\003\001\004\001\
\005\001\006\001\007\001\007\001\015\001\015\001\017\001\018\001\
\019\001\016\001\016\001\039\001\017\001\024\001\025\001\026\001\
\027\001\028\001\029\001\024\001\025\001\026\001\027\001\003\001\
\004\001\005\001\006\001\003\001\004\001\005\001\006\001\003\001\
\004\001\005\001\006\001\017\001\018\001\017\001\017\001\018\001\
\021\001\017\001\017\001\018\001\017\001\017\001\028\001\029\001\
\018\001\028\001\029\001\017\001\018\001\028\001\029\001\018\001\
\016\001\016\001\028\001\029\001\016\001\016\001\028\001\029\001\
\006\001\028\001\029\001\003\001\004\001\005\001\006\001\019\001\
\008\001\012\001\015\001\019\001\000\000\023\001\018\001\017\001\
\017\001\011\000\015\000\008\000\133\000\033\000"

let yynames_const = "\
  DOUBLE\000\
  RETURN\000\
  PLUS\000\
  MINUS\000\
  MULT\000\
  DIV\000\
  EGAL\000\
  COMMA\000\
  WHILE\000\
  FOR\000\
  IF\000\
  ELSE\000\
  FALSE\000\
  TRUE\000\
  SC\000\
  PARBEG\000\
  PAREND\000\
  ACCBEG\000\
  ACCEND\000\
  BRABEG\000\
  BRAEND\000\
  OPTIMIZE\000\
  STOP\000\
  EOF\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  AND\000\
  OR\000\
  NOT\000\
  NOP\000\
  SQRT\000\
  COS\000\
  SIN\000\
  EXP\000\
  LOG\000\
  INTOFBOOL\000\
  BOOLOFINT\000\
  "

let yynames_block = "\
  ID\000\
  FLOAT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'header) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 24 "./parser.mly"
                                ( let _ = funList := _2 in _1 )
# 338 "./parser.ml"
               :  (((string * SalsaTypes.abstractValue) list) ref) ))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "./parser.mly"
                                             ( ref [] )
# 344 "./parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declList) in
    Obj.repr(
# 27 "./parser.mly"
                                                             ( ref _1 )
# 351 "./parser.ml"
               : 'header))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cst) in
    Obj.repr(
# 29 "./parser.mly"
                                                               ( (ssaIzeRHS _1,_3)::[] )
# 359 "./parser.ml"
               : 'declList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'cst) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'declList) in
    Obj.repr(
# 30 "./parser.mly"
                                                              ( (ssaIzeRHS _1,_3)::_4 )
# 368 "./parser.ml"
               : 'declList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 32 "./parser.mly"
                                  ( Assign(ssaIzeLHS _1,_3,Lab(!codeLine)) )
# 376 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "./parser.mly"
               ( Nop(Lab(!codeLine)) )
# 382 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 34 "./parser.mly"
                     ( Seq(_1,_3,Lab(!codeLine)) )
# 390 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'bexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'cmd) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 35 "./parser.mly"
                                                           ( genCond _2 _4 _8 (Lab(!codeLine)) )
# 399 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 36 "./parser.mly"
                                                      ( genCond _2 _4 (Nop(Lab(!codeLine))) (Lab(!codeLine)) )
# 407 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'bexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 37 "./parser.mly"
                                               ( try genWhile _2 _4 (Lab(!codeLine)) with _ -> raise (Error "ntnt"))
# 415 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 38 "./parser.mly"
                           ( _2 )
# 422 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 39 "./parser.mly"
                                                    ( _1 )
# 429 "./parser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cst) in
    Obj.repr(
# 41 "./parser.mly"
                            ( Cst(_1,Lab(!codeLine)) )
# 436 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "./parser.mly"
                                                              ( Id(ssaIzeRHS _1,Lab(!codeLine)) )
# 443 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "./parser.mly"
                                                    ( Plus(_1,_3,Lab(!codeLine)) )
# 451 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "./parser.mly"
                                         ( Minus(_1,_3,Lab(!codeLine)) )
# 459 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "./parser.mly"
                                        ( Times(_1,_3,Lab(!codeLine)) )
# 467 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "./parser.mly"
                                       ( Div(_1,_3,Lab(!codeLine)) )
# 475 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "./parser.mly"
                            ( Uminus(_2,Lab(!codeLine)) )
# 482 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 48 "./parser.mly"
                                ( Sqrt(_3,Lab(!codeLine)) )
# 489 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "./parser.mly"
                               ( Cos(_3,Lab(!codeLine)) )
# 496 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "./parser.mly"
                               ( Sin(_3,Lab(!codeLine)) )
# 503 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 51 "./parser.mly"
                               ( Exp(_3,Lab(!codeLine)) )
# 510 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 52 "./parser.mly"
                               ( Log(_3,Lab(!codeLine)) )
# 517 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'bexpr) in
    Obj.repr(
# 53 "./parser.mly"
                                      ( IntOfBool(_3,Lab(!codeLine)) )
# 524 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprList) in
    Obj.repr(
# 54 "./parser.mly"
                                                           ( FunCall(_1, _3, Lab(!codeLine)) )
# 532 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "./parser.mly"
                                   ( _2 )
# 539 "./parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "./parser.mly"
                                         ( BCst(true,Lab(!codeLine)) )
# 545 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "./parser.mly"
                                                              ( BCst(false,Lab(!codeLine)) )
# 551 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "./parser.mly"
                                      ( Lt(_1,_3,Lab(!codeLine)) )
# 559 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "./parser.mly"
                                       ( Lte(_1,_3,Lab(!codeLine)) )
# 567 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "./parser.mly"
                                      ( Gt(_1,_3,Lab(!codeLine)) )
# 575 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "./parser.mly"
                                       ( Gte(_1,_3,Lab(!codeLine)) )
# 583 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "./parser.mly"
                                         ( Eq(_1,_4,Lab(!codeLine)) )
# 591 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 64 "./parser.mly"
                                         ( And(_1,_3,Lab(!codeLine)) )
# 599 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 65 "./parser.mly"
                                        ( Or(_1,_3,Lab(!codeLine)) )
# 607 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 66 "./parser.mly"
                                           ( Not(_2,Lab(!codeLine)) )
# 614 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexpr) in
    Obj.repr(
# 67 "./parser.mly"
                                    ( _2 )
# 621 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "./parser.mly"
                                     ( BoolOfInt(_3,Lab(!codeLine)) )
# 628 "./parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'signedFloat) in
    Obj.repr(
# 70 "./parser.mly"
                         ( (I(_1,_1),ulp(I(_1,_1)))  )
# 635 "./parser.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'signedFloat) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'signedFloat) in
    Obj.repr(
# 71 "./parser.mly"
                                                           ( (I(_2,_4),ulp(I(_2,_4))) )
# 643 "./parser.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'signedFloat) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'signedFloat) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'signedFloat) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'signedFloat) in
    Obj.repr(
# 72 "./parser.mly"
                                                                                                ( (I(_2,_4),r_of_f (I(_6,_8))) )
# 653 "./parser.ml"
               : 'cst))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 74 "./parser.mly"
                           ( _1 )
# 660 "./parser.ml"
               : 'signedFloat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'paramList) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'cmd) in
    let _9 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 77 "./parser.mly"
                                                                                ( Func(_2, _4, _7, ssaIzeRHS _9, Lab(!codeLine)) )
# 670 "./parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : 'cmd) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 79 "./parser.mly"
                                                                                ( Func(_2,[] , _6, ssaIzeRHS _8, Lab(!codeLine)) )
# 679 "./parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "./parser.mly"
                                                                                ( _1::[] )
# 686 "./parser.ml"
               : 'exprList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprList) in
    Obj.repr(
# 82 "./parser.mly"
                                                                                ( _1::_3 )
# 694 "./parser.ml"
               : 'exprList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "./parser.mly"
                                                                                ( (ssaIzeRHS _2)::[] )
# 701 "./parser.ml"
               : 'paramList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'paramList) in
    Obj.repr(
# 85 "./parser.mly"
                                                                                ( (ssaIzeRHS _2)::_4 )
# 709 "./parser.ml"
               : 'paramList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 87 "./parser.mly"
                                                                                ( _1::[] )
# 716 "./parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 88 "./parser.mly"
                                                                                ( _1::_2 )
# 724 "./parser.ml"
               : 'prog))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  (((string * SalsaTypes.abstractValue) list) ref) )
;;
# 90 "./parser.mly"

# 751 "./parser.ml"
