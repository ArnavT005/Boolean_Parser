structure AST =
struct

type id = string

datatype binop = Add of int * int | Sub of int * int | Mul of int * int | And of int * int 
                | Or of int * int | Implies of int * int | Xor of int * int | Eq of int * int | Lt of int * int
                | Gt of int * int
datatype unary = Neg of int * int | Not of int * int

datatype Generic = INT | BOOL | N | ARROW of Generic * Generic

datatype decl = ValDecl of id * exp

and exp = NumConst of int
        | BoolConst of bool
        | VarExp of id
        | BinExp of binop * exp * exp
        | UnExp of unary * exp
        | ConExp of exp * exp * exp
        | LetExp of decl * exp
        | AbsExp of Generic * id * Generic * exp * Generic
        | AppExp of exp * exp
        | NullExp
             
datatype value =  IntVal of int
                | BoolVal of bool
                | Lambda of Generic * id * Generic * exp * Generic * (id * value) list
                | NA


datatype TYPE =   TYPESAFE of Generic
              |   TYPEB of binop * Generic * Generic
              |   TYPEU of unary * Generic 
              |   TYPEC of Generic * Generic * Generic
              |   TYPELAbs of Generic * Generic
              |   TYPELApp of Generic * Generic

type valEnvironment = (id * value) list
type typEnvironment = (id * TYPE) list

fun first(Add(a, _)) = a
|   first(Sub(a, _)) = a
|   first(Mul(a, _)) = a
|   first(And(a, _)) = a
|   first(Or(a, _)) = a
|   first(Implies(a, _)) = a
|   first(Xor(a, _)) = a
|   first(Eq(a, _)) = a
|   first(Lt(a, _)) = a
|   first(Gt(a, _)) = a

fun second(Add(_, b)) = b
|   second(Sub(_, b)) = b
|   second(Mul(_, b)) = b
|   second(And(_, b)) = b
|   second(Or(_, b)) = b
|   second(Implies(_, b)) = b
|   second(Xor(_, b)) = b
|   second(Eq(_, b)) = b
|   second(Lt(_, b)) = b
|   second(Gt(_, b)) = b

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

fun printAST(NumConst i) = "NumExp(" ^ Int.toString(i) ^ ")"
|   printAST(BoolConst b) = "BoolExp(" ^ Bool.toString(b) ^ ")"
|   printAST(VarExp(name)) = "VarExp(\"" ^ name ^ "\")"
|   printAST(BinExp(bin, exp1, exp2)) = (
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
|   printAST(UnExp(un, exp1)) = (
      case un of
        Neg(_, _) => "UnExp(NEGATE, " ^ printAST(exp1) ^ ")"
      | Not(_, _) => "UnExp(NOT, " ^ printAST(exp1) ^ ")"
    )
|   printAST(ConExp(exp1, exp2, exp3)) = "ConExp(" ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ", " ^ printAST(exp3) ^ ")"
|   printAST(LetExp(decl1, exp1)) = "LetExp(" ^ printDec(decl1) ^ ", " ^ printAST(exp1) ^ ")"
|   printAST(AbsExp(g1, x, gx, e, ge)) = "Fn(\"" ^ x ^ "\", " ^ printGen(gx) ^ ", " ^ printGen(ge) ^ ", " ^ printAST(e) ^ ")" 
|   printAST(AppExp(exp1, exp2)) = "AppExp(" ^ printAST(exp1) ^ ", " ^ printAST(exp2) ^ ")"
|   printAST(NullExp) = ""
and
    printDec(ValDecl(id1, exp1)) = "ValDecl(\"" ^ id1 ^ "\", " ^ printAST(exp1) ^ ")"


fun printf(ID, AbsExp(g, x, gx, e, ge)) = "Fun(\"" ^ ID ^ "\", \"" ^ x ^ "\", " ^ printGen(gx) ^ ", " ^ printGen(ge) ^ ", " ^ printAST(e) ^ ")"
| printf(_) = ""  


fun typeToString(TYPEB(b, g1, g2)) =
    let val str = "Type Error:" ^ Int.toString(first(b)) ^ ":" ^ Int.toString(second(b)) ^ ":"
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
      str ^ str2 ^ got
    end
|   typeToString(TYPEU(u, g)) = 
    let val str = "Type Error:" ^ Int.toString(firstU(u)) ^ ":" ^ Int.toString(secondU(u)) ^ ":"
        val got = ", but got " ^ printGen(g);
        val str2 = (
          case u of
            Not(_, _) => "NOT: Expected BOOL"
          | Neg(_, _) => "NEGATE: Expected INT"
        )  
    in
      str ^ str2 ^ got
    end          
|   typeToString(TYPEC(g1, g2, g3)) =
    let val str = "Type Error:IF-THEN-ELSE: Expected BOOL * BOOL * BOOL or BOOL * INT * INT";
        val got = ", but got " ^ printGen(g1) ^ " * " ^ printGen(g2) ^ " * " ^ printGen(g3)
    in
        str ^ got
    end
|   typeToString(TYPELAbs(g1, g2)) = 
    let val str = "Type Error:FunDef: Expected " ^ printGen(g1);
        val got = ", but got " ^ printGen(g2)
    in
        str ^ got
    end
|   typeToString(TYPELApp(g1, g2)) =
    let val str = "Type Error:FunApp: Expected ARROW(A, B) * A"
        val got = ", but got " ^ printGen(g1) ^ " * " ^ printGen(g2)
    in
        str ^ got
    end
|   typeToString(TYPESAFE(g)) = "Type Safe: " ^ printGen(g);                                     


fun valToString(IntVal i) = Int.toString(i)
|   valToString(BoolVal b) = Bool.toString(b)
|   valToString(NA)        = "Value of expression cannot be determined"
|   valToString(Lambda(g1, x, gx, e, ge, env)) = "Lambda type value"


fun evalExp(e:exp, env:valEnvironment) =
    case e of
        NumConst i                  => IntVal i
      | BoolConst b                 => BoolVal b  
      | VarExp x                    => valEnvLookup(x, env)       
      | BinExp (b, e1, e2)          => evalBinExp(b, e1, e2, env)
      | UnExp(u, e)                 => evalUnExp(u, e, env)
      | ConExp(e1, e2, e3)          => evalTerExp(e1, e2, e3, env)
      | LetExp(ValDecl(x, e1), e2)  =>
          let val v = evalExp(e1, env)
          in
               evalExp(e2, valEnvAdd(x, v, env))
          end      
      | AbsExp(g1, x, gx, e, ge)    => Lambda(g1, x, gx, e, ge, env)
      | AppExp(e1, e2)              =>
          let val v2 = evalExp(e2, env);
              val v1 = evalExp(e1, env)
          in
            case v1 of
              IntVal i                      => NA
            | BoolVal b                     => NA
            | NA                            => NA
            | Lambda(g, x, gx, e, ge, ev)   => evalExp(e, valEnvAdd(x, v2, ev)@env)
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
    case e of
        NumConst i                  => TYPESAFE(INT)
      | BoolConst b                 => TYPESAFE(BOOL) 
      | VarExp x                    => typEnvLookup(x, env)       
      | BinExp (b, e1, e2)          => typeCheckBinExp(b, e1, e2, env)
      | UnExp(u, e)                 => typeCheckUnExp(u, e, env)
      | ConExp(e1, e2, e3)          => typeCheckTerExp(e1, e2, e3, env)
      | LetExp(ValDecl(x, e1), e2)  =>
          let val t = typeCheckExp(e1, env)
          in
              case t of
                TYPEB(_, _, _)  => t
              | TYPEU(_, _)     => t
              | TYPEC(_, _, _)  => t 
              | TYPELAbs(_, _)  => t
              | TYPELApp(_, _)  => t
              | TYPESAFE(g)     => typeCheckExp(e2, typEnvAdd(x, t, env))
          end      
      | AbsExp(g1, x, gx, e, ge)    => 
            let val tx = TYPESAFE(gx)
            in
                let val te = typeCheckExp(e, typEnvAdd(x, tx, env))
                in
                  case te of
                    TYPEB(_, _, _) => te
                  | TYPEU(_, _)    => te 
                  | TYPEC(_, _, _) => te
                  | TYPELAbs(_, _) => te
                  | TYPELApp(_, _) => te
                  | TYPESAFE(g)    => 
                      if(g = ge) then TYPESAFE(g1)
                      else TYPELAbs(g1, ARROW(gx, g))
                end
            end          
      | AppExp(e1, e2)             =>
            let val t2 = typeCheckExp(e2, env)
            in
              case t2 of
                  TYPEB(_, _, _) => t2
                | TYPEU(_, _)    => t2
                | TYPEC(_, _, _) => t2
                | TYPELAbs(_, _) => t2
                | TYPELApp(_, _) => t2
                | TYPESAFE(g2)   =>
                    let val t1 = typeCheckExp(e1, env)
                    in
                      case t1 of
                          TYPEB(_, _, _) => t1
                        | TYPEU(_, _)    => t1
                        | TYPEC(_, _, _) => t1
                        | TYPELAbs(_, _) => t1
                        | TYPELApp(_, _) => t1
                        | TYPESAFE(g1)   => (
                            case g1 of 
                                INT           => TYPELApp(INT, g2)
                              | BOOL          => TYPELApp(BOOL, g2)
                              | N             => TYPELApp(N, g2)
                              | ARROW(gf, gs) => if(gf = g2) then TYPESAFE(gs) else TYPELApp(g1, g2)
                        )   
                    end 
            end                        
      | NullExp   => TYPESAFE(N)  
and
typeCheckBinExp(b:binop, e1:exp, e2:exp, env:typEnvironment) =
    let val t1 = typeCheckExp(e1, env);
        val t2 = typeCheckExp(e2, env)
    in    
      case t1 of
           TYPEB(_, _, _) => t1
         | TYPEU(_, _)    => t1
         | TYPEC(_, _, _) => t1
         | TYPELAbs(_, _) => t1
         | TYPELApp(_, _) => t1
         | TYPESAFE(g1)   => (
              case t2 of
                TYPEB(_, _, _) => t2
              | TYPEU(_, _)    => t2
              | TYPEC(_, _, _) => t2
              | TYPELAbs(_, _) => t2
              | TYPELApp(_, _) => t2
              | TYPESAFE(g2)   => (
                  case (b, g1, g2) of
                     (Add(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Add(l, c), _, _)           => TYPEB(Add(l, c), g1, g2)
                    |(Sub(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Sub(l, c), _, _)           => TYPEB(Sub(l, c), g1, g2) 
                    |(Mul(_, _), INT, INT)       => TYPESAFE(INT)
                    |(Mul(l, c), _, _)           => TYPEB(Mul(l, c), g1, g2) 
                    |(Lt(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Lt(l, c), _, _)            => TYPEB(Lt(l, c), g1, g2)
                    |(Gt(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Gt(l, c), _, _)            => TYPEB(Gt(l, c), g1, g2) 
                    |(And(_, _), BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(And(l, c), _, _)           => TYPEB(And(l, c), g1, g2) 
                    |(Or(_, _), BOOL, BOOL)      => TYPESAFE(BOOL)
                    |(Or(l, c), _, _)            => TYPEB(Or(l, c), g1, g2) 
                    |(Xor(_, _), BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(Xor(l, c), _, _)           => TYPEB(Xor(l, c), g1, g2)  
                    |(Implies(_, _), BOOL, BOOL) => TYPESAFE(BOOL)
                    |(Implies(l, c), _, _)       => TYPEB(Implies(l, c), g1, g2)  
                    |(Eq(_, _), INT, INT)        => TYPESAFE(BOOL)
                    |(Eq(_, _), BOOL, BOOL)      => TYPESAFE(BOOL)  
                    |(Eq(l, c), _, _)            => TYPEB(Eq(l, c), g1, g2)  
              )
          )
    end   
and
typeCheckUnExp(u:unary, e:exp, env:typEnvironment) =
    let val t = typeCheckExp(e, env)
    in  
      case t of
          TYPEB(_, _, _)  => t
         |TYPEU(_, _)     => t
         |TYPEC(_, _, _)  => t
         |TYPELAbs(_, _)  => t
         |TYPELApp(_, _)  => t 
         |TYPESAFE(g)     => (
            case (u, g) of
                (Neg(_, _), INT)  => TYPESAFE(INT)
               |(Neg(l, c), _)    => TYPEU(Neg(l, c), g)
               |(Not(_, _), BOOL) => TYPESAFE(BOOL)
               |(Not(l, c), _)    => TYPEU(Not(l, c), g)
          )
    end             
and
typeCheckTerExp(e1:exp, e2:exp, e3:exp, env:typEnvironment) =
    let val t1 = typeCheckExp(e1, env);
        val t2 = typeCheckExp(e2, env);
        val t3 = typeCheckExp(e3, env)
    in   
      case t1 of 
          TYPEB(_, _, _)  => t1
         |TYPEU(_, _)     => t1
         |TYPEC(_, _, _)  => t1
         |TYPELAbs(_, _)  => t1
         |TYPELApp(_, _)  => t1 
         |TYPESAFE(g1)    => (
              case t2 of
                  TYPEB(_, _, _)  => t2
                 |TYPEU(_, _)     => t2
                 |TYPEC(_, _, _)  => t2
                 |TYPELAbs(_, _)  => t2
                 |TYPELApp(_, _)  => t2 
                 |TYPESAFE(g2)    => (
                    case t3 of 
                      TYPEB(_, _, _)  => t3
                     |TYPEU(_, _)     => t3
                     |TYPEC(_, _, _)  => t3
                     |TYPELAbs(_, _)  => t3
                     |TYPELApp(_, _)  => t3
                     |TYPESAFE(g3)    => (
                          if(g2 <> g3) then TYPEC(g1, g2, g3)
                          else
                            if(g1 = BOOL) then TYPESAFE(g2)
                            else TYPEC(g1, g2, g3)
                      ) 
                  )
          )       
    end       
end
