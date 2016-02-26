open Num ;; 

type finterval = Empty
               | I of float * float 
;;

type rinterval = JEmpty
               | J of float * float 
               | JInfty
;;
                            
type abstractValue = finterval * rinterval ;;

type phiNode = Phi of string * string * string ;;

type label = Lab of int ;;

type expression =
  Id of string * label
| Cst of  abstractValue * label
| Plus of expression * expression * label
| Minus of expression * expression * label
| Times of expression * expression * label
| Div of expression * expression * label
| Uminus of expression * label
| Sqrt of expression * label
| Cos of expression * label
| Sin of expression * label
| Exp of expression * label
| Log of expression * label
| IntOfBool of boolExpression * label
| FunCall of string * (expression list) * label 


and boolExpression =
  BCst of bool * label
| Eq  of expression * expression * label
| Lt  of expression * expression * label
| Lte of expression * expression * label
| Gt  of expression * expression * label 
| Gte of expression * expression * label 
| And of boolExpression * boolExpression * label 
| Or  of boolExpression * boolExpression * label 
| Not of boolExpression * label 
| BoolOfInt of expression * label  
;;

type command =
  Assign of string * expression * label
| Nop of label
| Seq of command * command * label
| Cond of boolExpression * command * command * (phiNode list)  * label
| While of boolExpression * command * (phiNode list) * label
;;



type func = 
Func of string * (string list) * command * string * label ;;
 

type context =
  Hole
| CtxNop
| CtxAssign of string * expression
| CtxSeq of context * context 
| CtxCond of boolExpression * context * context * (phiNode list)
| CtxWhile of boolExpression * context * (phiNode list)
;;


type funRule = 
  Inline 
| Value
| Slice
| Formal 
;;


(*******************************************************)


type qExpression =
  QId of string * label
| QCst of  num * label
| QPlus of qExpression * qExpression * label
| QMinus of qExpression * qExpression * label
| QTimes of qExpression * qExpression * label
| QDiv of qExpression * qExpression * label
| QUminus of qExpression * label
| QSqrt of qExpression * label
| QCos of qExpression * label
| QSin of qExpression * label
| QExp of qExpression * label 
| QLog of qExpression * label 
| QIntOfBool of qBoolExpression * label 


and qBoolExpression =
  QBCst of bool * label
| QEq of qExpression * qExpression * label
| QLt of qExpression * qExpression * label
| QLte of qExpression * qExpression * label
| QGt of qExpression * qExpression * label 
| QGte of qExpression * qExpression * label 
| QAnd of qBoolExpression * qBoolExpression * label 
| QOr of qBoolExpression * qBoolExpression * label 
| QNot of qBoolExpression * label 
| QBoolOfInt of qExpression * label  
;;

type qCommand =
  QAssign of string * qExpression * label
| QNop of label
| QSeq of qCommand * qCommand * label
| QCond of qBoolExpression * qCommand * qCommand * label
| QWhile of qBoolExpression * qCommand * label
;;

(********************************************************)

type fExpression =
  FId of string * label
| FCst of  float * label
| FPlus of fExpression * fExpression * label
| FMinus of fExpression * fExpression * label
| FTimes of fExpression * fExpression * label
| FDiv of fExpression * fExpression * label
| FUminus of fExpression * label
| FSqrt of fExpression * label
| FCos of fExpression * label
| FSin of fExpression * label
| FLog of fExpression * label 
| FExp of fExpression * label 
| FIntOfBool of fBoolExpression * label 


and fBoolExpression =
  FBCst of bool * label
| FEq of fExpression * fExpression * label
| FLt of fExpression * fExpression * label
| FLte of fExpression * fExpression * label
| FGt of fExpression * fExpression * label 
| FGte of fExpression * fExpression * label 
| FAnd of fBoolExpression * fBoolExpression * label 
| FOr of fBoolExpression * fBoolExpression * label 
| FNot of fBoolExpression * label 
| FBoolOfInt of fExpression * label  
;;

type fCommand =
  FAssign of string * fExpression * label
| FNop of label
| FSeq of fCommand * fCommand * label
| FCond of fBoolExpression * fCommand * fCommand * label
| FWhile of fBoolExpression * fCommand * label
;;







