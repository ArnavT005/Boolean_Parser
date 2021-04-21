structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | And | Or | Implies | Xor | Eq | Lt | Gt
datatype unary = Neg | Not

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
              |   TYPEC of Generic * Generic * Generic
              |   TYPELAbs of Generic
              |   TYPELApp of Generic * Generic

type valEnvironment = (id * value) list
type typEnvironment = (id * TYPE) list

(* fun first (a, _) = a;
fun second (_, b) = b;

fun firstArg(ARROW(a, _)) = a
|   firstArg(_) = N; 

fun secondArg(ARROW(_, b)) = b
|   secondArg(_) = N; 

fun Type(TYPESAFE(x)) = x
|   Type(_) = N;   *)

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
      | AbsExp(g1, x, gx, e, ge)    => Lambda(g1, x, gx, e, ge)
      | AppExp(e1, e2)              =>
          let val v2 = evalExp(e2, env);
              val v1 = evalExp(e1, env)
          in
            case v1 of
              IntVal i                 => NA
            | BoolVal b                => NA
            | NA                       => NA
            | Lambda(g, x, gx, e, ge)  => evalExp(e, valEnvAdd(x, v2, env))
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
              | TYPELAbs(_)     => t
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
                  | TYPELAbs(_)    => te
                  | TYPELApp(_)    => te
                  | TYPESAFE(g)    => 
                      if(g = ge) then TYPESAFE(ge)
                      else TYPELAbs(g)
                end
            end          
      | AppExp(e1, e2)             =>
            let val t2 = typeCheckExp(e2, env)
            in
              case t2 of
                  TYPEB(_, _, _) => t2
                | TYPEU(_, _)    => t2
                | TYPEC(_, _, _) => t2
                | TYPELAbs(_)    => t2
                | TYPELApp(_, _) => t2
                | TYPESAFE(g2)   =>
                    let val t1 = typeCheckExp(e1, env)
                    in
                      case t1 of
                          TYPEB(_, _, _) => t1
                        | TYPEU(_, _)    => t1
                        | TYPEC(_, _, _) => t1
                        | TYPELAbs(_)    => t1
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
         | TYPELAbs(_)    => t1
         | TYPELApp(_, _) => t1
         | TYPESAFE(g1)   => (
              case t2 of
                TYPEB(_, _, _) => t2
              | TYPEU(_, _)    => t2
              | TYPEC(_, _, _) => t2
              | TYPELAbs(_)    => t2
              | TYPELApp(_, _) => t2
              | TYPESAFE(g2)   => (
                  case (b, g1, g2) of
                     (Add, INT, INT)       => TYPESAFE(INT)
                    |(Add, _, _)           => TYPEB(Add, g1, g2)
                    |(Sub, INT, INT)       => TYPESAFE(INT)
                    |(Sub, _, _)           => TYPEB(Sub, g1, g2) 
                    |(Mul, INT, INT)       => TYPESAFE(INT)
                    |(Mul, _, _)           => TYPEB(Sub, g1, g2) 
                    |(Lt, INT, INT)        => TYPESAFE(BOOL)
                    |(Lt, _, _)            => TYPEB(Lt, g1, g2)
                    |(Gt, INT, INT)        => TYPESAFE(BOOL)
                    |(Gt, _, _)            => TYPEB(Gt, g1, g2) 
                    |(And, BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(And, _, _)           => TYPEB(And, g1, g2) 
                    |(Or, BOOL, BOOL)      => TYPESAFE(BOOL)
                    |(Or, _, _)            => TYPEB(Or, g1, g2) 
                    |(Xor, BOOL, BOOL)     => TYPESAFE(BOOL)
                    |(Xor, _, _)           => TYPEB(Xor, g1, g2)  
                    |(Implies, BOOL, BOOL) => TYPESAFE(BOOL)
                    |(Implies, _, _)       => TYPEB(Implies, g1, g2)  
                    |(Eq, INT, INT)        => TYPESAFE(BOOL)
                    |(Eq, BOOL, BOOL)      => TYPESAFE(BOOL)  
                    |(Eq, _, _)            => TYPEB(Eq, g1, g2)  
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
         |TYPELAbs(_)     => t
         |TYPELApp(_, _)  => t 
         |TYPESAFE(g)     => (
            case (u, g) of
                (Neg, INT)  => TYPESAFE(INT)
               |(Neg, _)    => TYPEU(Neg, g)
               |(Not, BOOL) => TYPESAFE(BOOL)
               |(Not, _)    => TYPEU(Not, g)
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
         |TYPELAbs(_)     => t1
         |TYPELApp(_, _)  => t1 
         |TYPESAFE(g1)    => (
              case t2 of
                  TYPEB(_, _, _)  => t2
                 |TYPEU(_, _)     => t2
                 |TYPEC(_, _, _)  => t2
                 |TYPELAbs(_)     => t2
                 |TYPELApp(_, _)  => t2 
                 |TYPESAFE(g2)     => (
                    case t3 of 
                      TYPEB(_, _, _)  => t3
                     |TYPEU(_, _)     => t3
                     |TYPEC(_, _, _)  => t3
                     |TYPELAbs(_)     => t3
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
