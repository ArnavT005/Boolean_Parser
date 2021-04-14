structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"
exception 

fun evalExp(e:exp, env:environment):value =
    case e of
	      NumConst i            => IntVal i
      | BoolConst b           => BoolVal b  
      | VarExp x              => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnExp(u, e)         => evalUnExp (u, e, env)
      | LetExp(ValDecl(x, e1), e2)  =>
  	       let val v1 = evalExp (e1, env)
	         in
	             evalExp(e2, envAdd (x, v1, env))
           end		   
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env)) of
        (Add, IntVal i1, IntVal i2)        => IntVal (i1 + i2)
       |(Add, _, _)                        => raise
       |(Sub, IntVal i1, IntVal i2)        => IntVal (i1 - i2)
       |(Add, _, _)                        => raise)
       |(Mul, IntVal i1, IntVal i2)        => IntVal (i1 * i2)
       |(Add, _, _)                        => raise)
       |(And, BoolVal b1, BoolVal b2)      => BoolVal (b1 andalso b2)
       |(And, _, _)                        => raise)
       |(Or, BoolVal b1, BoolVal b2)       => BoolVal (b1 orelse b2)
       |(Or, _, _)                         => raise)
       |(Eq, BoolVal b1, BoolVal b2)       => BoolVal (b1 = b2)
       |(Eq, IntVal i1, IntVal i2)         => BoolVal (i1 = i2)
       |(Eq, _, _)                         => raise)
       |(Xor, BoolVal b1, BoolVal b2)      => BoolVal (if b1 then (not b2) else b2)
       |(Xor, _, _)                        => raise)
       |(Implies, BoolVal b1, BoolVal b2)  => BoolVal ((not b1) orelse b2)
       |(Implies, _, _)                    => raise)
       |(Lt, IntVal i1, IntVal i2)         => BoolVal (i1 < i2)
       |(Lt, _, _)                         => raise)
       |(Gt, IntVal i1, IntVal i2)         => BoolVal (i1 > i2)
       |(Gt, _, _)                         => raise)
    end
and
evalUnExp(u:unary, e:exp, env:environment):value =
    case (u, evalExp(e, env)) of
        (Neg, IntVal i) => IntVal (~i)
      | (Neg, _) => raise
      | (Not, BoolVal b) => BoolVal (not b)
      | (Not, _) => raise
    end
      

