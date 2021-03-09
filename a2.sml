signature boolean_TOKENS =
sig
type ('a,'b) token
type svalue
val ID: (string) *  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val IMPLIES:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val error:  'a * 'a -> (svalue,'a) token
val XOR:  'a * 'a -> (svalue,'a) token
val OR:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val CONST: (string) *  'a * 'a -> (svalue,'a) token
val TERM:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature boolean_LRVALS=
sig
structure Tokens : boolean_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
functor booleanLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : boolean_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
val post_order = ref ""
val syntaxError = ref ""

val ifError = ref (false: bool)

fun IntStarInt(a, b) = Int.toString(a) ^ ":" ^ Int.toString(b) ^ ":";

fun StringStarIntStarInt(str, a, b) = Int.toString(a) ^ ":" ^ Int.toString(b) ^ ":";

fun ToString(str, a: int, b: int) = str;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\016\000\005\000\015\000\006\000\014\000\007\000\013\000\
\\009\000\012\000\010\000\011\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\019\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\021\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\023\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\025\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\027\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\030\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\032\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\038\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\008\000\042\000\011\000\008\000\
\\014\000\007\000\016\000\006\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\011\000\008\000\014\000\007\000\
\\016\000\006\000\000\000\
\\001\000\005\000\015\000\006\000\014\000\007\000\013\000\008\000\034\000\
\\009\000\012\000\010\000\011\000\015\000\033\000\000\000\
\\001\000\005\000\015\000\006\000\014\000\007\000\013\000\008\000\036\000\
\\009\000\012\000\010\000\011\000\012\000\035\000\000\000\
\\001\000\005\000\015\000\006\000\014\000\007\000\013\000\008\000\040\000\
\\009\000\012\000\010\000\011\000\013\000\039\000\000\000\
\\044\000\003\000\010\000\004\000\009\000\011\000\008\000\014\000\007\000\
\\016\000\006\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\\049\000\000\000\
\\050\000\000\000\
\\051\000\000\000\
\\052\000\005\000\015\000\006\000\014\000\007\000\013\000\009\000\012\000\
\\010\000\011\000\000\000\
\\053\000\000\000\
\\054\000\000\000\
\\055\000\000\000\
\\056\000\000\000\
\\057\000\005\000\015\000\006\000\014\000\007\000\013\000\009\000\012\000\
\\010\000\011\000\000\000\
\\058\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\"
val actionRowNumbers =
"\011\000\001\000\016\000\015\000\
\\020\000\002\000\003\000\004\000\
\\019\000\005\000\006\000\011\000\
\\007\000\008\000\018\000\017\000\
\\012\000\029\000\013\000\036\000\
\\022\000\031\000\023\000\032\000\
\\027\000\035\000\026\000\025\000\
\\034\000\024\000\033\000\021\000\
\\030\000\009\000\037\000\014\000\
\\038\000\010\000\039\000\028\000\
\\040\000\000\000"
val gotoT =
"\
\\001\000\041\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\015\000\004\000\001\000\000\000\
\\000\000\
\\004\000\016\000\000\000\
\\004\000\018\000\000\000\
\\004\000\020\000\000\000\
\\000\000\
\\004\000\022\000\000\000\
\\004\000\024\000\000\000\
\\004\000\026\000\000\000\
\\004\000\027\000\000\000\
\\004\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 42
val numrules = 26
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | CONST of unit ->  (string)
 | formula of unit ->  (string) | statement of unit ->  (string)
 | stmt_list of unit ->  (string) | program of unit ->  (unit)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
(nil
 $$ (T 0),nil
 $$ (T 7))::
(nil
 $$ (T 1),nil
 $$ (T 7))::
(nil
 $$ (T 3),nil
 $$ (T 7))::
(nil
 $$ (T 4),nil
 $$ (T 7))::
(nil
 $$ (T 5),nil
 $$ (T 7))::
(nil
 $$ (T 6),nil
 $$ (T 7))::
(nil
 $$ (T 8),nil
 $$ (T 7))::
(nil
 $$ (T 9),nil
 $$ (T 7))::
(nil
 $$ (T 10),nil
 $$ (T 7))::
(nil
 $$ (T 11),nil
 $$ (T 7))::
(nil
 $$ (T 12),nil
 $$ (T 7))::
(nil
 $$ (T 13),nil
 $$ (T 7))::
(nil
 $$ (T 14),nil
 $$ (T 7))::
(nil
 $$ (T 15),nil
 $$ (T 7))::
(nil
 $$ (T 2),nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "TERM"
  | (T 2) => "CONST"
  | (T 3) => "NOT"
  | (T 4) => "AND"
  | (T 5) => "OR"
  | (T 6) => "XOR"
  | (T 7) => "error"
  | (T 8) => "EQUALS"
  | (T 9) => "IMPLIES"
  | (T 10) => "IF"
  | (T 11) => "THEN"
  | (T 12) => "ELSE"
  | (T 13) => "LPAREN"
  | (T 14) => "RPAREN"
  | (T 15) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.stmt_list stmt_list1, stmt_list1left, 
stmt_list1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (stmt_list as stmt_list1) = stmt_list1 ()
 in (
if(not(!ifError)) then (post_order := stmt_list1 ^ ", " ^ "program -> stmt_list\n"; print(!post_order)) else print(!syntaxError)
)
end)
 in ( LrTable.NT 0, ( result, stmt_list1left, stmt_list1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statement statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.stmt_list
 (fn _ => let val  (statement as statement1) = statement1 ()
 in (
post_order := statement1 ^ ", " ^ "stmt_list -> statement"; !post_order
)
end)
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.statement statement1, _, statement1right))
 :: ( _, ( MlyValue.stmt_list stmt_list1, stmt_list1left, _)) :: 
rest671)) => let val  result = MlyValue.stmt_list (fn _ => let val  (
stmt_list as stmt_list1) = stmt_list1 ()
 val  (statement as statement1) = statement1 ()
 in (
post_order := stmt_list1 ^ ", " ^ statement1 ^ ", " ^ "stmt_list -> stmt_list statement"; !post_order
)
end)
 in ( LrTable.NT 1, ( result, stmt_list1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 in (
post_order := formula ^ ", " ^ "TERM \";\", " ^ "statement -> formula TERM"; !post_order
)
end)
 in ( LrTable.NT 2, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  (
CONST as CONST1) = CONST1 ()
 in (
post_order := "CONST \"" ^ CONST ^ "\", " ^ "formula -> CONST"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (
post_order := "ID \"" ^ ID ^ "\", " ^ "formula -> ID"; !post_order)

end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 6, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula as formula1
) = formula1 ()
 in (
post_order := "LPAREN \"(\", " ^ formula1 ^ ", " ^ "RPAREN \")\", " ^ "formula -> LPAREN formula RPAREN"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _,
 ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (formula as formula1) = formula1 ()
 in (
post_order := "NOT \"NOT\", " ^ formula1 ^ ", " ^ "formula -> NOT formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (formula as 
formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
post_order := formula1 ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ formula2 ^ ", " ^ "formula -> formula IMPLIES formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (formula as 
formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
post_order := formula1 ^ ", " ^ "AND \"AND\", " ^ formula2 ^ ", " ^ "formula -> formula AND formula" ; !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (formula as 
formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
post_order := formula1 ^ ", " ^ "OR \"OR\", " ^ formula2 ^ ", " ^ "formula -> formula OR formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (formula as 
formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
post_order := formula1 ^ ", " ^ "XOR \"XOR\", " ^ formula2 ^ ", " ^ "formula -> formula XOR formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (formula as 
formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
post_order := formula1 ^ ", " ^ "EQUALS \"EQUALS\", " ^ formula2 ^ ", " ^ "formula -> formula EQUALS formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.formula formula3, _, formula3right)) :: _
 :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _, ( 
MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.formula (fn _ => let val  (formula
 as formula1) = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in (
post_order := "IF \"IF\", " ^ formula1 ^ ", " ^ "THEN \"THEN\", " ^ formula2 ^ ", " ^ "ELSE \"ELSE\", " ^ formula3 ^ ", " ^ "formula -> IF formula THEN formula ELSE formula"; !post_order
)
end)
 in ( LrTable.NT 3, ( result, IF1left, formula3right), rest671)
end
|  ( 14, ( ( _, ( _, _, error1right)) :: ( _, ( _, LPAREN1left, _)) ::
 rest671)) => let val  result = MlyValue.formula (fn _ => (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> LPAREN formula RPAREN\"\n"; ifError := true; !post_order) else !post_order
))
 in ( LrTable.NT 3, ( result, LPAREN1left, error1right), rest671)
end
|  ( 15, ( ( _, ( _, _, error1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (formula as formula1
) = formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> LPAREN formula RPAREN\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, error1right), rest671)
end
|  ( 16, ( ( _, ( _, _, error1right)) :: ( _, ( _, NOT1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => (
if(not(!ifError)) then (syntaxError := "Syntax Error:"(*IntStarInt(error)*) ^ "\"formula -> NOT formula\"\n"; ifError := true; !post_order) else !post_order
))
 in ( LrTable.NT 3, ( result, NOT1left, error1right), rest671)
end
|  ( 17, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> formula IMPLIES formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, error1right), rest671)
end
|  ( 18, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> formula AND formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, error1right), rest671)
end
|  ( 19, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> formula OR formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, error1right), rest671)
end
|  ( 20, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> formula EQUALS formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, formula1left, error1right), rest671)
end
|  ( 21, ( ( _, ( _, _, error1right)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"; ifError := true; !post_order) else !post_order
))
 in ( LrTable.NT 3, ( result, IF1left, error1right), rest671)
end
|  ( 22, ( ( _, ( _, _, error1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  
result = MlyValue.formula (fn _ => let val  (formula as formula1) = 
formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, IF1left, error1right), rest671)
end
|  ( 23, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  
result = MlyValue.formula (fn _ => let val  (formula as formula1) = 
formula1 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, IF1left, error1right), rest671)
end
|  ( 24, ( ( _, ( _, _, error1right)) :: ( _, ( MlyValue.formula 
formula2, _, _)) :: _ :: ( _, ( MlyValue.formula formula1, _, _)) :: (
 _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"; ifError := true; print(!syntaxError); !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, IF1left, error1right), rest671)
end
|  ( 25, ( ( _, ( _, _, error1right)) :: _ :: ( _, ( MlyValue.formula 
formula2, _, _)) :: _ :: ( _, ( MlyValue.formula formula1, _, _)) :: (
 _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 val  formula2 = formula2 ()
 in (
if(not(!ifError)) then (syntaxError := "Syntax Error:" (*IntStarInt(error)*) ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"; ifError := true; !post_order) else !post_order
)
end)
 in ( LrTable.NT 3, ( result, IF1left, error1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : boolean_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun error (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
functor booleanLexFun(structure Tokens: boolean_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1

val token_list = ref ([]: string list)

fun reverse(ls) = 
	let fun rev_iter([], ls) = ls
		|	rev_iter(x :: xs, ls) = rev_iter(xs, x :: ls)
	in
		rev_iter(ls, [])
	end
	
fun append list str1 str2 = 
		list := "\"" ^ str2 ^ "\"" :: str1 :: !list
		
fun secondaryPrint([]) = print ""
|	secondaryPrint(ls) =
		(print (", " ^ hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun printString([]) = print ""
|	printString(ls) = 
		(print (hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun error(x, y, str) = (TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(x) ^ ":" ^ Int.toString(y) ^ ":" ^ str ^ "\n"); OS.Process.exit(OS.Process.success));

fun colinc(x, str) = (x := !x + String.size str)			   

val eof = fn () => (print "["; printString(reverse(!token_list)); print "]\n"; Tokens.EOF(!lineNum, !columnNum))	


end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\048\050\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\048\003\003\003\003\003\003\003\047\046\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\045\003\003\003\003\
\\003\042\004\004\004\033\029\004\004\021\004\004\004\004\018\016\
\\004\004\004\004\009\004\004\004\006\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\007\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\008\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\013\005\005\005\005\005\005\005\
\\005\005\010\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\011\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\012\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\014\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\015\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\017\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\019\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\020\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\028\005\005\005\005\005\005\022\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\023\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\024\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\025\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\026\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (26, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\027\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\030\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\031\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\032\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\039\005\005\005\
\\005\034\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\035\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\036\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\037\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\038\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\040\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (40, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\041\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\043\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\044\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
 (48, 
"\000\000\000\000\000\000\000\000\000\049\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\049\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 68)], trans = 0},
{fin = [(N 66),(N 68)], trans = 4},
{fin = [(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 6},
{fin = [(N 66)], trans = 7},
{fin = [(N 25),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 9},
{fin = [(N 66)], trans = 10},
{fin = [(N 66)], trans = 11},
{fin = [(N 63),(N 66)], trans = 4},
{fin = [(N 66)], trans = 13},
{fin = [(N 66)], trans = 14},
{fin = [(N 48),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 16},
{fin = [(N 21),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 18},
{fin = [(N 66)], trans = 19},
{fin = [(N 14),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 21},
{fin = [(N 66)], trans = 22},
{fin = [(N 66)], trans = 23},
{fin = [(N 66)], trans = 24},
{fin = [(N 66)], trans = 25},
{fin = [(N 66)], trans = 26},
{fin = [(N 40),(N 66)], trans = 4},
{fin = [(N 43),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 29},
{fin = [(N 66)], trans = 30},
{fin = [(N 66)], trans = 31},
{fin = [(N 66)], trans = 11},
{fin = [(N 66),(N 68)], trans = 33},
{fin = [(N 66)], trans = 34},
{fin = [(N 66)], trans = 35},
{fin = [(N 66)], trans = 36},
{fin = [(N 66)], trans = 37},
{fin = [(N 32),(N 66)], trans = 4},
{fin = [(N 66)], trans = 39},
{fin = [(N 66)], trans = 40},
{fin = [(N 53),(N 66)], trans = 4},
{fin = [(N 66),(N 68)], trans = 42},
{fin = [(N 66)], trans = 43},
{fin = [(N 18),(N 66)], trans = 4},
{fin = [(N 6),(N 68)], trans = 0},
{fin = [(N 10),(N 68)], trans = 0},
{fin = [(N 8),(N 68)], trans = 0},
{fin = [(N 4),(N 68)], trans = 48},
{fin = [(N 4)], trans = 48},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNum := !lineNum + 1; columnNum := 1; lex())
| 10 => let val yytext=yymktext() in append token_list "RPAREN" ")"; colinc(columnNum, yytext); T.RPAREN(!lineNum, !columnNum - 1) end
| 14 => let val yytext=yymktext() in append token_list "NOT" "NOT"; colinc(columnNum, yytext); T.NOT(!lineNum, !columnNum - 3) end
| 18 => let val yytext=yymktext() in append token_list "AND" "AND"; colinc(columnNum, yytext); T.AND(!lineNum, !columnNum - 3) end
| 21 => let val yytext=yymktext() in append token_list "OR" "OR"; colinc(columnNum, yytext); T.OR(!lineNum, !columnNum - 2) end
| 25 => let val yytext=yymktext() in append token_list "XOR" "XOR"; colinc(columnNum, yytext); T.XOR(!lineNum, !columnNum - 3) end
| 32 => let val yytext=yymktext() in append token_list "EQUALS" "EQUALS"; colinc(columnNum, yytext); T.EQUALS(!lineNum, !columnNum - 6) end
| 4 => let val yytext=yymktext() in colinc(columnNum, yytext); lex() end
| 40 => let val yytext=yymktext() in append token_list "IMPLIES" "IMPLIES"; colinc(columnNum, yytext); T.IMPLIES(!lineNum, !columnNum - 7) end
| 43 => let val yytext=yymktext() in append token_list "IF" "IF"; colinc(columnNum, yytext); T.IF(!lineNum, !columnNum - 2) end
| 48 => let val yytext=yymktext() in append token_list "THEN" "THEN"; colinc(columnNum, yytext); T.THEN(!lineNum, !columnNum - 4) end
| 53 => let val yytext=yymktext() in append token_list "ELSE" "ELSE"; colinc(columnNum, yytext); T.ELSE(!lineNum, !columnNum - 4) end
| 6 => let val yytext=yymktext() in append token_list "TERM" ";"; colinc(columnNum, yytext); T.TERM(!lineNum, !columnNum - 1) end
| 63 => let val yytext=yymktext() in append token_list "CONST" yytext; colinc(columnNum, yytext); T.CONST(yytext, !lineNum, !columnNum - String.size yytext) end
| 66 => let val yytext=yymktext() in append token_list "ID" yytext; colinc(columnNum, yytext); T.ID(yytext, !lineNum, !columnNum - String.size yytext) end
| 68 => let val yytext=yymktext() in error(!lineNum, !columnNum, yytext); colinc(columnNum, yytext); lex() end
| 8 => let val yytext=yymktext() in append token_list "LPAREN" "("; colinc(columnNum, yytext); T.LPAREN(!lineNum, !columnNum - 1) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
structure booleanLrVals = booleanLrValsFun(structure Token = LrParser.Token)
structure booleanLex = booleanLexFun(structure Tokens = booleanLrVals.Tokens);
structure booleanParser =
	Join(structure LrParser = LrParser
		 structure ParserData = booleanLrVals.ParserData
		 structure Lex = booleanLex)
		 
fun invoke lexstream =
	let fun print_error(str, pos1, pos2) =
			TextIO.output(TextIO.stdOut, "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ str ^ "\n")
	in
		booleanParser.parse(0, lexstream, print_error, ())
	end

fun stringToLexer str =
	let val done = ref false;
		val lexer = booleanParser.makeLexer(fn _ => if(!done) then "" else (done := true; str))
	in
		lexer
	end
	
fun fileToString fileName = 
	let val instream = TextIO.openIn fileName
	in
		TextIO.input instream
	end

fun lexerToParser lexer = 
	let val dummyEOF = booleanLrVals.Tokens.EOF(0, 0);
		val (result, lexer) = invoke lexer;
		val (nextToken, lexer) = booleanParser.Stream.get lexer
	in
		if(booleanParser.sameToken(nextToken, dummyEOF)) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed Tokens.\n"); result)
	end	
	
(*val fileName = CommandLine.arguments();
val str = fileToString (hd(fileName));*)
val parseString = lexerToParser o stringToLexer;	 
(*val lexer = stringToLexer str;
lexerToParser lexer;*)
