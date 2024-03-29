
val post_order = ref ""
val statNum = ref 0
(*storing global variables*)
val funcValEnv = ref ([]: AST.valEnvironment)
val funcTypEnv = ref ([]: AST.typEnvironment)

fun first(f, _) = f
fun second(_, s) = s


fun boolToString(b) = 
	if(b) then "TRUE"
	else "FALSE"

%%

%name boolean

%term EOF | TERM | CONST_Bool of bool * AST.double | CONST_Int of int * AST.double | NOT of int * int | AND of int * int | OR of int * int | XOR of int * int |
	  EQUALS of int * int | IMPLIES of int * int | IF of AST.cord | THEN | ELSE | FI of AST.cord | LPAREN | RPAREN | ID of AST.id * AST.double |
	  PLUS of int * int | MINUS of int * int | TIMES of int * int | NEGATE of int * int | LESSTHAN of int * int | GREATERTHAN of int * int | 
	  LET of AST.cord | IN | END of AST.cord | EQ | COLON | FN of int * int | FUN of int * int | ARROW | DEF | INT | BOOL

(*Add new operator tokens and other keywords*)

%nonterm program of unit | stmt_list of string * string | statement of string * string | 
		 formula of string * AST.exp | declaration of string * AST.decl | typ of string * AST.Generic

%pos int * int * int
%eop EOF
%noshift EOF

%right DEF EQ
%right IMPLIES
%left EQUALS OR XOR AND
%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NOT NEGATE 
%right ARROW

%start program

%verbose

%%


program: stmt_list (
			post_order := first(stmt_list) ^ ", " ^ "program -> stmt_list\n"; 
			print("Parser Output\n\n" ^ !post_order); 
			print("\nSemantic Analysis\n\n" ^ second(stmt_list) ^ "\n\n")
		)

stmt_list: statement (
		   	  post_order := first(statement) ^ ", " ^ "stmt_list -> statement"; 
			  (!post_order, second(statement))
		 )
		 | stmt_list TERM statement (
		 	  post_order := first(stmt_list) ^ ", " ^ "TERM \";\", " ^ first(statement) ^ ", " ^ "stmt_list -> stmt_list TERM statement"; 
		 	  (!post_order, second(stmt_list) ^ "\n\n" ^ second(statement))
		 )

statement: formula (
			  post_order := first(formula) ^ ", " ^ "statement -> formula";
			  statNum := !statNum + 1;
			  let val t = AST.typeCheckExp(second(formula), !funcTypEnv);
			  	  val v = AST.evalExp(second(formula), !funcValEnv);
			  	  val strType = AST.typeToString(t)
			  	  val strValue = (
			  	  	case t of
			  	  		 AST.TYPESAFE(g) => AST.valToString(v)
			  	  		|_               => AST.valToString(AST.NA)
			  	  )
			  in 
			  	(!post_order, "Statement:" ^ Int.toString(!statNum) ^ ":\n" ^ "Abstract Syntax Tree: " ^ AST.printAST(second(formula)) ^ "\n" ^ strType ^ "\nEvaluated Value: " ^ strValue)
			  end		
		 )
		| FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FUN \"fun\", ID \"" ^ first(ID1) ^ "\", LPAREN \"(\", ID \"" ^ first(ID2) ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\", " ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula";
			statNum := !statNum + 1;
			let val initialEnv = ref [];
				val v = AST.Lambda(AST.ARROW(second(typ1), second(typ2)), first(ID2), second(typ1), second(formula), second(typ2), initialEnv);
				val enclosingEnv = (first(ID1), v) :: !funcValEnv;
				val () = initialEnv := enclosingEnv;
				val t = AST.typeCheckExp(AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), first(ID2), second(typ1), second(formula), second(typ2), FUN, second(AST.expCord(second(formula)))), (first(ID1), AST.TYPESAFE(AST.ARROW(second(typ1), second(typ2)))) :: !funcTypEnv);
				val strType = AST.typeToString(t)
			in (
				funcValEnv := (first(ID1), v) :: !funcValEnv;
				funcTypEnv := (first(ID1), t) :: !funcTypEnv;
				(!post_order, "Statement:" ^ Int.toString(!statNum) ^ ":\n" ^ "Abstract Syntax Tree: " ^ AST.printf(first(ID1), AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), first(ID2), second(typ1), second(formula), second(typ2), FUN, second(AST.expCord(second(formula))))) ^ "\n" ^ strType)
			)
			end	
		)

declaration: ID EQ formula (
				post_order := "ID \"" ^ first(ID) ^ "\", EQ \"=\", " ^ first(formula) ^ ", declaration -> ID EQ formula"; 
				(!post_order, AST.ValDecl(first(ID), second(formula)))
			)

formula: CONST_Bool (
			post_order := "CONST_Bool \"" ^ boolToString(first(CONST_Bool)) ^ "\", " ^ "formula -> CONST_Bool"; 
			(!post_order, AST.BoolConst(first(CONST_Bool), first(second(CONST_Bool)), second(second(CONST_Bool))))
		)
	   | CONST_Int (
	   		post_order := "CONST_Int \"" ^ Int.toString (first(CONST_Int)) ^ "\", " ^ "formula -> CONST_Int"; 
	   		(!post_order, AST.NumConst(first(CONST_Int), first(second(CONST_Int)), second(second(CONST_Int))))
	   	)
	   | ID (
	   		post_order := "ID \"" ^ first(ID) ^ "\", " ^ "formula -> ID"; 
	   		(!post_order, AST.VarExp(first(ID), first(second(ID)), second(second(ID))))
	    )
	   | LPAREN formula RPAREN (
	   		post_order := "LPAREN \"(\", " ^ first(formula) ^ ", " ^ "RPAREN \")\", " ^ "formula -> LPAREN formula RPAREN"; 
	   		(!post_order, second(formula))
	   	)
	   | NOT formula (
	   		post_order := "NOT \"NOT\", " ^ first(formula) ^ ", " ^ "formula -> NOT formula"; 
	   		(!post_order, AST.UnExp(AST.Not(NOT), second(formula), NOT, second(AST.expCord(second(formula)))))
	   	)
	   | formula IMPLIES formula (
	   		post_order := first(formula1) ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ first(formula2) ^ ", " ^ "formula -> formula IMPLIES formula"; 
	   		(!post_order,AST.BinExp(AST.Implies(IMPLIES), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula AND formula (
	   		post_order := first(formula1) ^ ", " ^ "AND \"AND\", " ^ first(formula2) ^ ", " ^ "formula -> formula AND formula" ; 
	   		(!post_order,AST.BinExp(AST.And(AND), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula OR formula (
	   		post_order := first(formula1) ^ ", " ^ "OR \"OR\", " ^ first(formula2) ^ ", " ^ "formula -> formula OR formula"; 
	   		(!post_order,AST.BinExp(AST.Or(OR), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula XOR formula (
	   		post_order := first(formula1) ^ ", " ^ "XOR \"XOR\", " ^ first(formula2) ^ ", " ^ "formula -> formula XOR formula"; 
	   		(!post_order,AST.BinExp(AST.Xor(XOR), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula EQUALS formula (
	   		post_order := first(formula1) ^ ", " ^ "EQUALS \"EQUALS\", " ^ first(formula2) ^ ", " ^ "formula -> formula EQUALS formula"; 
	   		(!post_order,AST.BinExp(AST.Eq(EQUALS), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | IF formula THEN formula ELSE formula FI (
	   		post_order := "IF \"if\", " ^ first(formula1) ^ ", " ^ "THEN \"then\", " ^ first(formula2) ^ ", " ^ "ELSE \"else\", " ^ first(formula3) ^ ", FI \"fi\", " ^ "formula -> IF formula THEN formula ELSE formula FI"; 
	   		(!post_order, AST.ConExp(second(formula1), second(formula2), second(formula3), IF, FI))
	   	)
	   | LET declaration IN formula END (
	   		post_order := "LET \"let\", " ^ first(declaration) ^ ", IN \"in\", " ^ first(formula) ^ ", END \"end\", " ^ "formula -> LET declaration IN formula END"; 
	   		(!post_order, AST.LetExp(second(declaration), second(formula), LET, END))
	   	)
	   | formula PLUS formula (
	   		post_order := first(formula1) ^ ", " ^ "PLUS \"PLUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula PLUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Add(PLUS), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula MINUS formula (
	   		post_order := first(formula1) ^ ", " ^ "MINUS \"MINUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula MINUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Sub(MINUS), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula TIMES formula (
	   		post_order := first(formula1) ^ ", " ^ "TIMES \"TIMES\", " ^ first(formula2) ^ ", " ^ "formula -> formula TIMES formula" ; 
	   		(!post_order,AST.BinExp(AST.Mul(TIMES), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | NEGATE formula (
	   		post_order := "NEGATE \"NEGATE\", " ^ first(formula) ^ ", " ^ "formula -> NEGATE formula"; 
	   		(!post_order, AST.UnExp(AST.Neg(NEGATE), second(formula), NEGATE, second(AST.expCord(second(formula)))))
	   	)
	   | formula LESSTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "LESSTHAN \"LESSTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula LESSTHAN formula" ; 
	   		(!post_order, AST.BinExp(AST.Lt(LESSTHAN), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | formula GREATERTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "GREATERTHAN \"GREATERTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula GREATERTHAN formula" ; 
	   		(!post_order, AST.BinExp(AST.Gt(GREATERTHAN), second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   	)
	   | LPAREN formula formula RPAREN (
	   		post_order := "LPAREN \"(\", " ^ first(formula1) ^ ", " ^ first(formula2) ^ ", RPAREN \")\", formula -> LPAREN formula formula RPAREN";
	   		(!post_order, AST.AppExp(second(formula1), second(formula2), first(AST.expCord(second(formula1))), second(AST.expCord(second(formula2)))))
	   )
	   | FN LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FN \"fn\", LPAREN \"(\", ID \"" ^ first(ID) ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\", " ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula";
			(!post_order, AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), first(ID), second(typ1), second(formula), second(typ2), FN, second(AST.expCord(second(formula)))))
		) 

typ:	INT (post_order := "INT \"int\", typ -> INT"; (!post_order, AST.INT))
	|   BOOL (post_order := "BOOL \"bool\", typ -> BOOL"; (!post_order, AST.BOOL))
	|   typ ARROW typ (
		post_order := first(typ1) ^ ", ARROW \"->\", " ^ first(typ2) ^ ", typ -> typ ARROW typ";
		(!post_order, AST.ARROW(second(typ1), second(typ2)))
	) 
	|   LPAREN typ RPAREN (
		post_order := "LPAREN \"(\", " ^ first(typ) ^ ", RPAREN \")\", typ -> LPAREN typ RPAREN";
		(!post_order, second(typ))
	)