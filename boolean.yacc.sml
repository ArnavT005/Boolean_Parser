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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\011\000\003\000\010\000\004\000\009\000\010\000\008\000\
\\013\000\007\000\015\000\006\000\000\000\
\\001\000\002\000\017\000\005\000\016\000\006\000\015\000\007\000\014\000\
\\008\000\013\000\009\000\012\000\000\000\
\\001\000\003\000\010\000\004\000\009\000\010\000\008\000\013\000\007\000\
\\015\000\006\000\000\000\
\\001\000\005\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\011\000\028\000\000\000\
\\001\000\005\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\012\000\030\000\000\000\
\\001\000\005\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\014\000\027\000\000\000\
\\033\000\002\000\011\000\003\000\010\000\004\000\009\000\010\000\008\000\
\\013\000\007\000\015\000\006\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\005\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\000\000\
\\043\000\000\000\
\\044\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\005\000\016\000\006\000\015\000\007\000\014\000\008\000\013\000\
\\009\000\012\000\000\000\
\"
val actionRowNumbers =
"\001\000\002\000\008\000\007\000\
\\013\000\003\000\003\000\003\000\
\\012\000\011\000\003\000\003\000\
\\003\000\003\000\003\000\010\000\
\\009\000\006\000\004\000\015\000\
\\016\000\020\000\019\000\018\000\
\\017\000\014\000\003\000\005\000\
\\003\000\021\000\000\000"
val gotoT =
"\
\\001\000\030\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\016\000\004\000\001\000\000\000\
\\000\000\
\\004\000\017\000\000\000\
\\004\000\018\000\000\000\
\\004\000\019\000\000\000\
\\000\000\
\\000\000\
\\004\000\020\000\000\000\
\\004\000\021\000\000\000\
\\004\000\022\000\000\000\
\\004\000\023\000\000\000\
\\004\000\024\000\000\000\
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
\\004\000\027\000\000\000\
\\000\000\
\\004\000\029\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 31
val numrules = 15
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
 | ID of unit ->  (string)
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
  | (T 7) => "EQUALS"
  | (T 8) => "IMPLIES"
  | (T 9) => "IF"
  | (T 10) => "THEN"
  | (T 11) => "ELSE"
  | (T 12) => "LPAREN"
  | (T 13) => "RPAREN"
  | (T 14) => "ID"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID stmt_list1, stmt_list1left, 
stmt_list1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  stmt_list1 = stmt_list1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, stmt_list1left, stmt_list1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID statement1, statement1left, 
statement1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  statement1 = statement1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, statement1left, statement1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.ntVOID statement1, _, statement1right)) :: (
 _, ( MlyValue.ntVOID stmt_list1, stmt_list1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  stmt_list1 = 
stmt_list1 ()
 val  statement1 = statement1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, stmt_list1left, statement1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.ntVOID formula1
, formula1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  formula1 = formula1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( _, TERM1left, TERM1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 2, ( result, TERM1left, TERM1right), rest671)
end
|  ( 5, ( ( _, ( _, CONST1left, CONST1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = formula1
 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID formula1, _, formula1right)) :: ( _, 
( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  formula1 = formula1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID formula2, _, formula2right)) :: _ :: 
( _, ( MlyValue.ntVOID formula1, formula1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = formula1
 ()
 val  formula2 = formula2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.ntVOID formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.ntVOID formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.ntVOID formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.ntVOID formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.ntVOID formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.ntVOID formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.ntVOID formula2, _, formula2right)) :: _ ::
 ( _, ( MlyValue.ntVOID formula1, formula1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.ntVOID formula3, _, formula3right)) :: _ ::
 ( _, ( MlyValue.ntVOID formula2, _, _)) :: _ :: ( _, ( 
MlyValue.ntVOID formula1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)
) => let val  result = MlyValue.ntVOID (fn _ => ( let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, IF1left, formula3right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
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
fun CONST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
end
end
