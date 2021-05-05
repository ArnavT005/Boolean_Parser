structure AST =
struct

type id = string
type cord = int * int
type double = cord * cord

datatype binop = Add of int * int | Sub of int * int | Mul of int * int | And of int * int 
                | Or of int * int | Implies of int * int | Xor of int * int | Eq of int * int
                | Lt of int * int | Gt of int * int

datatype unary = Neg of int * int | Not of int * int

datatype Generic = INT | BOOL | N | ARROW of Generic * Generic

datatype decl = ValDecl of id * exp

and exp = NumConst of int * cord * cord
        | BoolConst of bool * cord * cord
        | VarExp of id * cord * cord
        | BinExp of binop * exp * exp * cord * cord
        | UnExp of unary * exp * cord * cord
        | ConExp of exp * exp * exp * cord * cord
        | LetExp of decl * exp * cord * cord
        | AbsExp of Generic * id * Generic * exp * Generic * cord * cord
        | AppExp of exp * exp * cord * cord
        | NullExp
             
datatype value =  IntVal of int
                | BoolVal of bool
                | Lambda of Generic * id * Generic * exp * Generic * (id * value) list ref
                | NA


datatype TYPE =   TYPESAFE of Generic
              |   TYPEB of binop * Generic * Generic * cord * cord
              |   TYPEU of unary * Generic * cord * cord
              |   TYPEC of Generic * Generic * Generic * cord * cord
              |   TYPELAbs of Generic * Generic * cord * cord
              |   TYPELApp of Generic * Generic * cord * cord

type valEnvironment = (id * value) list
type typEnvironment = (id * TYPE) list

fun first(a, _) = a

fun second(_, b) = b

fun expCord(NumConst (i, c1, c2)) = (c1, c2)
|   expCord(BoolConst (b, c1, c2)) = (c1, c2)
|   expCord(VarExp (x, c1, c2)) = (c1, c2)
|   expCord(BinExp(b, e1, e2, c1, c2)) = (c1, c2)
|   expCord(UnExp(u, e1, c1, c2)) = (c1, c2)
|   expCord(ConExp(e1, e2, e3, c1, c2)) = (c1, c2)
|   expCord(LetExp(d, e, c1, c2)) = (c1, c2)
|   expCord(AbsExp(g, x, gx, e, ge, c1, c2)) = (c1, c2)
|   expCord(AppExp(e1, e2, c1, c2)) = (c1, c2)
|   expCord(NullExp) = ((0,0),(0,0)) 

fun firstB(Add(a, _)) = a
|   firstB(Sub(a, _)) = a
|   firstB(Mul(a, _)) = a
|   firstB(And(a, _)) = a
|   firstB(Or(a, _)) = a
|   firstB(Implies(a, _)) = a
|   firstB(Xor(a, _)) = a
|   firstB(Eq(a, _)) = a
|   firstB(Lt(a, _)) = a
|   firstB(Gt(a, _)) = a

fun secondB(Add(_, b)) = b
|   secondB(Sub(_, b)) = b
|   secondB(Mul(_, b)) = b
|   secondB(And(_, b)) = b
|   secondB(Or(_, b)) = b
|   secondB(Implies(_, b)) = b
|   secondB(Xor(_, b)) = b
|   secondB(Eq(_, b)) = b
|   secondB(Lt(_, b)) = b
|   secondB(Gt(_, b)) = b

fun firstU(Not(a, _)) = a
|   firstU(Neg(a, _)) = a

fun secondU(Not(_, b)) = b
|   secondU(Neg(_, b)) = b
 

fun valEnvAdd (var:id, v:value, env:valEnvironment) = (var, v)::env

fun typEnvAdd (var:id, t:TYPE, env:typEnvironment) = (var, t)::env    

fun valEnvLookup (var:id, env:valEnvironment) =
    case List.find(fn (x, _) => x = var) env of
        SOME (x, v) => v
    |   NONE        => NA

fun typEnvLookup (var:id, env:typEnvironment) =
    case List.find(fn (x, _) => x = var) env of
        SOME (x, t) => t
    |   NONE        => TYPESAFE(N)    


fun printGen(INT) = "INT"
|   printGen(BOOL) = "BOOL"
|   printGen(N)   = "N"
|   printGen(ARROW(g1, g2)) = "ARROW(" ^ printGen(g1) ^ ", " ^ printGen(g2) ^ ")" 

fun printAST(NumConst (i, _, _)) = "NumExp(" ^ Int.toString(i) ^ ")"
|   printAST(BoolConst (b, _, _)) = "BoolExp(" ^ Bool.toString(b) ^ ")"
|   printAST(VarExp(name, _, _)) = "VarExp(\"" ^ name ^ "\")"
|   printAST(BinExp(bin, exp1, exp2, _, _)) = (
      case bin of
          Add(_, _)     => "BinExp(PLUS, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"
        | Sub(_, _)     => "BinExp(MINUS, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Mul(_, _)     => "BinExp(TIMES, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | And(_, _)     => "BinExp(AND, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Or(_, _)      => "BinExp(OR, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Implies(_, _) => "BinExp(IMPLIES, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Xor(_, _)     => "BinExp(XOR, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Eq(_, _)      => "BinExp(EQUALS, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")" 
        | Lt(_, _)      => "BinExp(LESSTHAN, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"  
        | Gt(_, _)      => "BinExp(GREATERTHAN, " ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"
      )    
|   printAST(UnExp(un, exp1, _, _)) = (
      case un of
        Neg(_, _) => "UnExp(NEGATE, " ^ printAST(exp1) ^ ")"
      | Not(_, _) => "UnExp(NOT, " ^ printAST(exp1) ^ ")"
    )
|   printAST(ConExp(exp1, exp2, exp3, _, _)) = "ConExp(" ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ", " ^ printAST(exp3) ^ ")"
|   printAST(LetExp(decl1, exp1, _, _)) = "LetExp(" ^ printDec(decl1) ^ ", " ^ printAST(exp1) ^ ")"
|   printAST(AbsExp(g1, x, gx, e, ge, _, _)) = "Fn(\"" ^ x ^ "\", " ^ printGen(gx) ^ ", " ^ printGen(ge) ^ ", " ^ printAST(e) ^ ")" 
|   printAST(AppExp(exp1, exp2, _, _)) = "AppExp(" ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"
|   printAST(NullExp) = ""
and
    printDec(ValDecl(id1, exp1)) = "ValDecl(\"" ^ id1 ^ "\", " ^ printAST(exp1) ^ ")"


fun printf(ID, AbsExp(g, x, gx, e, ge, _, _)) = "Fun(\"" ^ ID ^ "\", \"" ^ x ^ "\", " ^ printGen(gx) ^ ", " ^ printGen(ge) ^ ", " ^ printAST(e) ^ ")"
| printf(_) = ""  


fun typeToString(TYPEB(b, g1, g2, c1, c2)) =
    let val str = "Type Error:Expression: Line " ^ Int.toString(first(c1)) ^ ", Column " ^ Int.toString(second(c1)) ^ " TO Line " ^ Int.toString(first(c2)) ^ ", Column " ^ Int.toString(second(c2)) ^ "\n";
        val oper = "Operator:" ^ Int.toString(firstB(b)) ^ ":" ^ Int.toString(secondB(b)) ^ ":"
        val got = ", but got " ^ printGen(g1) ^ " * " ^ printGen(g2);
        val str2 = (
          case b of
            Add(_, _) => "PLUS: Expected INT * INT"
          | Sub(_, _) => "MINUS: Expected INT * INT"
          | Mul(_, _) => "TIMES: Expected INT * INT"
          | Eq(_, _)  => "EQUALS: Expected INT * INT or BOOL * BOOL"
          | Lt(_, _)  => "LESSTHAN: Expected INT * INT"
          | Gt(_, _)  => "GREATERTHAN: Expected INT * INT"
          | And(_, _) => "AND: Expected BOOL * BOOL"
          | Or(_, _)  => "OR: Expected BOOL * BOOL"
          | Xor(_, _) => "XOR: Expected BOOL * BOOL"
          | Implies(_, _) => "IMPLIES: Expected BOOL * BOOL" 
        )  
    in
      str ^ oper ^ str2 ^ got
    end
|   typeToString(TYPEU(u, g, c1, c2)) = 
    let val str = "Type Error:" ^ "Expression: Line " ^ Int.toString(first(c1)) ^ ", Column " ^ Int.toString(second(c1)) ^ " TO Line " ^ Int.toString(first(c2)) ^ ", Column " ^ Int.toString(second(c2)) ^ "\n";
        val oper = "Operator:" ^ Int.toString(firstU(u)) ^ ":" ^ Int.toString(secondU(u)) ^ ":"
        val got = ", but got " ^ printGen(g);
        val str2 = (
          case u of
            Not(_, _) => "NOT: Expected BOOL"
          | Neg(_, _) => "NEGATE: Expected INT"
        )  
    in
      str ^ oper ^ str2 ^ got
    end          
|   typeToString(TYPEC(g1, g2, g3, c1, c2)) =
    let val str = "Type Error:Expression: Line " ^ Int.toString(first(c1)) ^ ", Column " ^ Int.toString(second(c1)) ^ " TO Line " ^ Int.toString(first(c2)) ^ ", Column " ^ Int.toString(second(c2)) ^ "\n";
        val oper = "Operator:IF-THEN-ELSE: Expected BOOL * BOOL * BOOL or BOOL * INT * INT"
        val got = ", but got " ^ printGen(g1) ^ " * " ^ printGen(g2) ^ " * " ^ printGen(g3)
    in
        str ^ oper ^ got
    end
|   typeToString(TYPELAbs(g1, g2, c1, c2)) = 
    let val str = "Type Error:Expression: Line " ^ Int.toString(first(c1)) ^ ", Column " ^ Int.toString(second(c1)) ^ " TO Line " ^ Int.toString(first(c2)) ^ ", Column " ^ Int.toString(second(c2)) ^ "\n";
        val oper = "Function:DEFINITION: Expected " ^ printGen(g1);
        val got = ", but got " ^ printGen(g2)
    in
        str ^ oper ^ got
    end
|   typeToString(TYPELApp(g1, g2, c1, c2)) =
    let val str = "Type Error:Expression: Line " ^ Int.toString(first(c1)) ^ ", Column " ^ Int.toString(second(c1)) ^ " TO Line " ^ Int.toString(first(c2)) ^ ", Column " ^ Int.toString(second(c2)) ^ "\n";
        val oper = "Function:APPLICATION: Expected ARROW(A, B) * A";
        val got = ", but got " ^ printGen(g1) ^ " * " ^ printGen(g2)
    in
        str ^ oper ^ got
    end
|   typeToString(TYPESAFE(g)) = "Type Safe: " ^ printGen(g);                                     


fun valToString(IntVal i) = Int.toString(i)
|   valToString(BoolVal b) = Bool.toString(b)
|   valToString(NA)        = "Value of expression cannot be determined"
|   valToString(Lambda(g1, x, gx, e, ge, env)) = "Lambda type value"


fun evalExp(e:exp, env:valEnvironment) =
    case e of
        NumConst (i, _, _)                => IntVal i
      | BoolConst (b, _, _)               => BoolVal b  
      | VarExp (x, _, _)                  => valEnvLookup(x, env)       
      | BinExp (b, e1, e2, _, _)          => evalBinExp(b, e1, e2, env)
      | UnExp(u, e, _, _)                 => evalUnExp(u, e, env)
      | ConExp(e1, e2, e3, _, _)          => evalTerExp(e1, e2, e3, env)
      | LetExp(ValDecl(x, e1), e2, _, _)  =>
          let val v = evalExp(e1, env)
          in
               evalExp(e2, valEnvAdd(x, v, env))
          end      
      | AbsExp(g1, x, gx, e, ge, _, _)    => Lambda(g1, x, gx, e, ge, ref env)
      | AppExp(e1, e2, _, _)              =>
          let val v2 = evalExp(e2, env);
              val v1 = evalExp(e1, env)
          in
            case v1 of
              IntVal i                      => NA
            | BoolVal b                     => NA
            | NA                            => NA
            | Lambda(g, x, gx, e, ge, ev)   => evalExp(e, valEnvAdd(x, v2, !ev))
          end    
      | _   => NA                          
and
evalBinExp(b:binop, e1:exp, e2:exp, env:valEnvironment) =
    let val v1 = evalExp(e1, env);
        val v2 = evalExp(e2, env)
    in    
      case (b, v1, v2) of
          (Add(_, _), IntVal i1, IntVal i2)       => IntVal (i1 + i2)
         |(Sub(_, _), IntVal i1, IntVal i2)       => IntVal (i1 - i2)
         |(Mul(_, _), IntVal i1, IntVal i2)       => IntVal (i1 * i2)
         |(Lt(_, _), IntVal i1, IntVal i2)        => BoolVal (i1 < i2)
         |(Gt(_, _), IntVal i1, IntVal i2)        => BoolVal (i1 > i2)
         |(And(_, _), BoolVal b1, BoolVal b2)     => BoolVal (b1 andalso b2)
         |(Or(_, _), BoolVal b1, BoolVal b2)      => BoolVal (b1 orelse b2)
         |(Xor(_, _), BoolVal b1, BoolVal b2)     => BoolVal (if b1 then (not b2) else b2)
         |(Implies(_, _), BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2)
         |(Eq(_, _), IntVal i1, IntVal i2)        => BoolVal (i1 = i2)
         |(Eq(_, _), BoolVal b1, BoolVal b2)      => BoolVal (b1 = b2)
         | _                => NA
    end     
and
evalUnExp(u:unary, e:exp, env:valEnvironment) =
    let val v = evalExp(e, env)
    in    
      case (u, v) of
          (Neg(_, _), IntVal i1)  => IntVal (~i1)
         |(Not(_, _), BoolVal b1) => BoolVal (not b1)
         | _                => NA
    end     
and
evalTerExp(e1:exp, e2:exp, e3:exp, env:valEnvironment) =
    let val v1 = evalExp(e1, env)
    in    
      case v1 of
          BoolVal b => 
            if b then evalExp(e2, env)
            else evalExp(e3, env)
         | _   => NA
    end     


fun typeCheckExp(e:exp, env:typEnvironment) =
    let val (c1, c2) = expCord(e)
    in
      case e of
          NumConst (i, _, _)                  => TYPESAFE(INT)
        | BoolConst (b, _, _)                 => TYPESAFE(BOOL) 
        | VarExp (x, _, _)                    => typEnvLookup(x, env)       
        | BinExp (b, e1, e2, _, _)          => typeCheckBinExp(b, e1, e2, env, c1, c2)
        | UnExp(u, e, _, _)                 => typeCheckUnExp(u, e, env, c1, c2)
        | ConExp(e1, e2, e3, _, _)          => typeCheckTerExp(e1, e2, e3, env, c1, c2)
        | LetExp(ValDecl(x, e1), e2, _, _)  =>
            let val t = typeCheckExp(e1, env)
            in
                case t of
                  TYPEB(_, _, _, _, _)  => t
                | TYPEU(_, _, _, _)     => t
                | TYPEC(_, _, _, _, _)  => t 
                | TYPELAbs(_, _, _, _)  => t
                | TYPELApp(_, _, _, _)  => t
                | TYPESAFE(g)     => typeCheckExp(e2, typEnvAdd(x, t, env))
            end      
        | AbsExp(g1, x, gx, e, ge, _, _)    => 
              let val tx = TYPESAFE(gx)
              in
                  let val te = typeCheckExp(e, typEnvAdd(x, tx, env))
                  in
                    case te of
                      TYPEB(_, _, _, _, _) => te
                    | TYPEU(_, _, _, _)    => te 
                    | TYPEC(_, _, _, _, _) => te
                    | TYPELAbs(_, _, _, _) => te
                    | TYPELApp(_, _, _, _) => te
                    | TYPESAFE(g)    => 
                        if(g = ge) then TYPESAFE(g1)
                        else TYPELAbs(g1, ARROW(gx, g), c1, c2)
                  end
              end          
        | AppExp(e1, e2, _, _)             =>
              let val t2 = typeCheckExp(e2, env)
              in
                case t2 of
                    TYPEB(_, _, _, _, _) => t2
                  | TYPEU(_, _, _, _)    => t2
                  | TYPEC(_, _, _, _, _) => t2
                  | TYPELAbs(_, _, _, _) => t2
                  | TYPELApp(_, _, _, _) => t2
                  | TYPESAFE(g2)   =>
                      let val t1 = typeCheckExp(e1, env)
                      in
                        case t1 of
                            TYPEB(_, _, _, _, _) => t1
                          | TYPEU(_, _, _, _)    => t1
                          | TYPEC(_, _, _, _, _) => t1
                          | TYPELAbs(_, _, _, _) => t1
                          | TYPELApp(_, _, _, _) => t1
                          | TYPESAFE(g1)   => (
                              case g1 of 
                                  INT           => TYPELApp(INT, g2, c1, c2)
                                | BOOL          => TYPELApp(BOOL, g2, c1, c2)
                                | N             => TYPELApp(N, g2, c1, c2)
                                | ARROW(gf, gs) => if(gf = g2) then TYPESAFE(gs) else TYPELApp(g1, g2, c1, c2)
                          )   
                      end 
              end                        
        | NullExp   => TYPESAFE(N)
    end      
and
typeCheckBinExp(b:binop, e1:exp, e2:exp, env:typEnvironment, c1:cord, c2:cord) =
    let val t1 = typeCheckExp(e1, env);
        val t2 = typeCheckExp(e2, env)
    in    
      case t1 of
           TYPEB(_, _, _, _, _) => t1
         | TYPEU(_, _, _, _)    => t1
         | TYPEC(_, _, _, _, _) => t1
         | TYPELAbs(_, _, _, _) => t1
         | TYPELApp(_, _, _, _) => t1
         | TYPESAFE(g1)   => (
              case t2 of
                TYPEB(_, _, _, _, _) => t2
              | TYPEU(_, _, _, _)    => t2
              | TYPEC(_, _, _, _, _) => t2
              | TYPELAbs(_, _, _, _) => t2
              | TYPELApp(_, _, _, _) => t2
              | TYPESAFE(g2)   => (
                  case (b, g1, g2) of
                     (Add(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Add(l, c), _, _)           => TYPEB(Add(l, c), g1, g2, c1, c2)
                    |(Sub(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Sub(l, c), _, _)           => TYPEB(Sub(l, c), g1, g2, c1, c2) 
                    |(Mul(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Mul(l, c), _, _)           => TYPEB(Mul(l, c), g1, g2, c1, c2) 
                    |(Lt(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Lt(l, c), _, _)            => TYPEB(Lt(l, c), g1, g2, c1, c2)
                    |(Gt(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Gt(l, c), _, _)            => TYPEB(Gt(l, c), g1, g2, c1, c2) 
                    |(And(_, _), BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(And(l, c), _, _)           => TYPEB(And(l, c), g1, g2, c1, c2) 
                    |(Or(_, _), BOOL, BOOL)      => TYPESAFE(BOOL)
                    |(Or(l, c), _, _)            => TYPEB(Or(l, c), g1, g2, c1, c2) 
                    |(Xor(_, _), BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(Xor(l, c), _, _)           => TYPEB(Xor(l, c), g1, g2, c1, c2)  
                    |(Implies(_, _), BOOL, BOOL) => TYPESAFE(BOOL)
                    |(Implies(l, c), _, _)       => TYPEB(Implies(l, c), g1, g2, c1, c2)  
                    |(Eq(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Eq(_, _), BOOL, BOOL)      => TYPESAFE(BOOL)  
                    |(Eq(l, c), _, _)            => TYPEB(Eq(l, c), g1, g2, c1, c2)  
              )
          )
    end   
and
typeCheckUnExp(u:unary, e:exp, env:typEnvironment, c1:cord, c2:cord) =
    let val t = typeCheckExp(e, env)
    in  
      case t of
          TYPEB(_, _, _, _, _)  => t
         |TYPEU(_, _, _, _)     => t
         |TYPEC(_, _, _, _, _)  => t
         |TYPELAbs(_, _, _, _)  => t
         |TYPELApp(_, _, _, _)  => t 
         |TYPESAFE(g)     => (
            case (u, g) of
                (Neg(_, _), INT)  => TYPESAFE(INT)
               |(Neg(l, c), _)    => TYPEU(Neg(l, c), g, c1, c2)
               |(Not(_, _), BOOL) => TYPESAFE(BOOL)
               |(Not(l, c), _)    => TYPEU(Not(l, c), g, c1, c2)
          )
    end             
and
typeCheckTerExp(e1:exp, e2:exp, e3:exp, env:typEnvironment, c1:cord, c2:cord) =
    let val t1 = typeCheckExp(e1, env);
        val t2 = typeCheckExp(e2, env);
        val t3 = typeCheckExp(e3, env)
    in   
      case t1 of 
          TYPEB(_, _, _, _, _)  => t1
         |TYPEU(_, _, _, _)     => t1
         |TYPEC(_, _, _, _, _)  => t1
         |TYPELAbs(_, _, _, _)  => t1
         |TYPELApp(_, _, _, _)  => t1 
         |TYPESAFE(g1)    => (
              case t2 of
                  TYPEB(_, _, _, _, _)  => t2
                 |TYPEU(_, _, _, _)     => t2
                 |TYPEC(_, _, _, _, _)  => t2
                 |TYPELAbs(_, _, _, _)  => t2
                 |TYPELApp(_, _, _, _)  => t2 
                 |TYPESAFE(g2)    => (
                    case t3 of 
                      TYPEB(_, _, _, _, _)  => t3
                     |TYPEU(_, _, _, _)     => t3
                     |TYPEC(_, _, _, _, _)  => t3
                     |TYPELAbs(_, _, _, _)  => t3
                     |TYPELApp(_, _, _, _)  => t3
                     |TYPESAFE(g3)    => (
                          if(g2 <> g3) then TYPEC(g1, g2, g3, c1, c2)
                          else
                            if(g1 = BOOL) then TYPESAFE(g2)
                            else TYPEC(g1, g2, g3, c1, c2)
                      ) 
                  )
          )       
    end       
end
