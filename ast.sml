structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | And | Or | Implies | Xor | Eq | Lt | Gt | LAMBDA
datatype unary = Neg | Not | IF | RET

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
                | Lambda of Generic * id * Generic * exp * Generic
                | NA


datatype TYPE =   TYPESAFE of Generic
              |   TYPEB of binop * Generic * Generic
              |   TYPEU of unary * Generic 

type valEnvironment = (id * value) list
type typEnvironment = (id * TYPE) list

fun first (a, _) = a;
fun second (_, b) = b;

fun firstArg(ARROW(a, _)) = a
|   firstArg(_) = N; 

fun secondArg(ARROW(_, b)) = b
|   secondArg(_) = N; 

fun Type(TYPESAFE(x)) = x
|   Type(_) = N;  

fun valEnvAdd (var:id, v:value, env:valEnvironment) =
    (var, v)::env

fun typEnvAdd (var:id, t:TYPE, env:typEnvironment) =
    (var, t)::env    

fun valEnvLookup (var:id, env:valEnvironment) =
    case List.find(fn (x, _) => x = var) env of
				SOME (x, v) => v
		|   NONE        => NA

fun typEnvLookup (var:id, env:typEnvironment) =
    case List.find(fn (x, _) => x = var) env of
        SOME (x, t) => t
    |   NONE        => TYPESAFE(N)    


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
      | AbsExp(g1, x, gx, e, ge)           => Lambda(g1, x, gx, e, ge)
      | AppExp(VarExp(f), e1)              =>
          let val v1 = evalExp(e1, env);
              val v2 = valEnvLookup(f, env)
          in
              case v2 of
                  IntVal i  => NA
                | BoolVal b => NA 
                | NA        => NA
                | Lambda(g1, x, gx, e2, ge) => evalExp(e2, valEnvAdd(x, v1, env))
          end
      | AppExp(AbsExp(g1, x, gx, e1, ge), e2) =>
          let val v = evalExp(e2, env)
          in
              evalExp(e1, valEnvAdd(x, v, env))
          end 
      | _   => NA                          

and
evalBinExp(b:binop, e1:exp, e2:exp, env:valEnvironment) =
    let val v1 = evalExp(e1, env);
        val v2 = evalExp(e2, env)
    in    
      case (b, v1, v2) of
          (Add, IntVal i1, IntVal i2)       => IntVal (i1 + i2)
         |(Sub, IntVal i1, IntVal i2)       => IntVal (i1 - i2)
         |(Mul, IntVal i1, IntVal i2)       => IntVal (i1 * i2)
         |(Lt, IntVal i1, IntVal i2)        => BoolVal (i1 < i2)
         |(Gt, IntVal i1, IntVal i2)        => BoolVal (i1 > i2)
         |(And, BoolVal b1, BoolVal b2)     => BoolVal (b1 andalso b2)
         |(Or, BoolVal b1, BoolVal b2)      => BoolVal (b1 orelse b2)
         |(Xor, BoolVal b1, BoolVal b2)     => BoolVal (if b1 then (not b2) else b2)
         |(Implies, BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2)
         |(Eq, IntVal i1, IntVal i2)        => BoolVal (i1 = i2)
         |(Eq, BoolVal b1, BoolVal b2)      => BoolVal (b1 = b2)
         | _                => NA
    end     
and
evalUnExp(u:unary, e:exp, env:valEnvironment) =
    let val v = evalExp(e, env)
    in    
      case (u, v) of
          (Neg, IntVal i1)  => IntVal (~i1)
         |(Not, BoolVal b1) => BoolVal (not b1)
         | _                => NA
    end     
and
evalTerExp(e1:exp, e2:exp, e3:exp, env:valEnvironment) =
    let val v1 = evalExp(e1, env);
        val v2 = evalExp(e2, env);
        val v3 = evalExp(e3, env)
    in    
      case (v1, v2, v3) of
          (BoolVal b, IntVal i1, IntVal i2)   => if b then IntVal i1 else IntVal i2
         |(BoolVal b, BoolVal b1, BoolVal b2) => if b then BoolVal b1 else BoolVal b2 
         | _                                  => NA
    end     
end