
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

%term EOF | TERM | CONST_Bool of bool | CONST_Int of int | NOT of int * int | AND of int * int | OR of int * int | XOR of int * int |
	  EQUALS of int * int | IMPLIES of int * int | IF | THEN | ELSE | FI | LPAREN | RPAREN | ID of AST.id |
	  PLUS of int * int | MINUS of int * int | TIMES of int * int | NEGATE of int * int | LESSTHAN of int * int | GREATERTHAN of int * int | 
	  LET | IN | END | EQ | COLON | FN | FUN | ARROW | DEF | INT | BOOL

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
			print(!post_order); 
			print("\n" ^ second(stmt_list) ^ "\n")
		)

stmt_list: statement (
		   	  post_order := first(statement) ^ ", " ^ "stmt_list -> statement"; 
			  (!post_order, second(statement))
		 )
		 | stmt_list TERM statement (
		 	  post_order := first(stmt_list) ^ ", " ^ "TERM \";\", " ^ first(statement) ^ ", " ^ "stmt_list -> stmt_list TERM statement"; 
		 	  (!post_order, second(stmt_list) ^ "\n" ^ second(statement))
		 )

statement: formula (
			  post_order := first(formula) ^ ", " ^ "statement -> formula";
			  statNum := !statNum + 1;
			  let val t = AST.typeCheckExp(second(formula), !funcTypEnv);
			  	  val v = AST.evalExp(second(formula), !funcValEnv)
			  	  val str = AST.typeToString(t)
			  in 
			  	(!post_order, "Statement:" ^ Int.toString(!statNum) ^ ":\n" ^ "Abstract Syntax Tree: " ^ AST.printAST(second(formula)) ^ "\n" ^ str ^ "\nEvaluated Value: " ^ AST.valToString(v))
			  end		
		 )
		| FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FUN \"fun\", ID \"" ^ ID1 ^ "\", LPAREN \"(\", ID \"" ^ ID2 ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\", " ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula";
			statNum := !statNum + 1;
			let val v = AST.Lambda(AST.ARROW(second(typ1), second(typ2)), ID2, second(typ1), second(formula), second(typ2), !funcValEnv);
				val t = AST.typeCheckExp(AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), ID2, second(typ1), second(formula), second(typ2)), (ID1, AST.TYPESAFE(AST.ARROW(second(typ1), second(typ2)))) :: !funcTypEnv);
				val str = AST.typeToString(t)
			in (
				funcValEnv := (ID1, v) :: !funcValEnv;
				funcTypEnv := (ID1, t) :: !funcTypEnv;
				(!post_order, "Statement:" ^ Int.toString(!statNum) ^ ":\n" ^ "Abstract Syntax Tree: " ^ AST.printf(ID1, AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), ID2, second(typ1), second(formula), second(typ2))) ^ "\n" ^ str)
			)
			end	
		)

declaration: ID EQ formula (
				post_order := "ID \"" ^ ID ^ "\", EQ \"=\", " ^ first(formula) ^ ", declaration -> ID EQ formula"; 
				(!post_order, AST.ValDecl(ID, second(formula)))
			)

formula: CONST_Bool (
			post_order := "CONST_Bool \"" ^ boolToString(CONST_Bool) ^ "\", " ^ "formula -> CONST_Bool"; 
			(!post_order, AST.BoolConst(CONST_Bool))
		)
	   | CONST_Int (
	   		post_order := "CONST_Int \"" ^ Int.toString (CONST_Int) ^ "\", " ^ "formula -> CONST_Int"; 
	   		(!post_order, AST.NumConst(CONST_Int))
	   	)
	   | ID (
	   		post_order := "ID \"" ^ ID ^ "\", " ^ "formula -> ID"; 
	   		(!post_order, AST.VarExp(ID))
	    )
	   | LPAREN formula RPAREN (
	   		post_order := "LPAREN \"(\", " ^ first(formula) ^ ", " ^ "RPAREN \")\", " ^ "formula -> LPAREN formula RPAREN"; 
	   		(!post_order, second(formula))
	   	)
	   | NOT formula (
	   		post_order := "NOT \"NOT\", " ^ first(formula) ^ ", " ^ "formula -> NOT formula"; 
	   		(!post_order, AST.UnExp(AST.Not(NOT), second(formula)))
	   	)
	   | formula IMPLIES formula (
	   		post_order := first(formula1) ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ first(formula2) ^ ", " ^ "formula -> formula IMPLIES formula"; 
	   		(!post_order,AST.BinExp(AST.Implies(IMPLIES), second(formula1), second(formula2)))
	   	)
	   | formula AND formula (
	   		post_order := first(formula1) ^ ", " ^ "AND \"AND\", " ^ first(formula2) ^ ", " ^ "formula -> formula AND formula" ; 
	   		(!post_order,AST.BinExp(AST.And(AND), second(formula1), second(formula2)))
	   	)
	   | formula OR formula (
	   		post_order := first(formula1) ^ ", " ^ "OR \"OR\", " ^ first(formula2) ^ ", " ^ "formula -> formula OR formula"; 
	   		(!post_order,AST.BinExp(AST.Or(OR), second(formula1), second(formula2)))
	   	)
	   | formula XOR formula (
	   		post_order := first(formula1) ^ ", " ^ "XOR \"XOR\", " ^ first(formula2) ^ ", " ^ "formula -> formula XOR formula"; 
	   		(!post_order,AST.BinExp(AST.Xor(XOR), second(formula1), second(formula2)))
	   	)
	   | formula EQUALS formula (
	   		post_order := first(formula1) ^ ", " ^ "EQUALS \"EQUALS\", " ^ first(formula2) ^ ", " ^ "formula -> formula EQUALS formula"; 
	   		(!post_order,AST.BinExp(AST.Eq(EQUALS), second(formula1), second(formula2)))
	   	)
	   | IF formula THEN formula ELSE formula FI (
	   		post_order := "IF \"if\", " ^ first(formula1) ^ ", " ^ "THEN \"then\", " ^ first(formula2) ^ ", " ^ "ELSE \"else\", " ^ first(formula3) ^ ", FI \"fi\", " ^ "formula -> IF formula THEN formula ELSE formula FI"; 
	   		(!post_order, AST.ConExp(second(formula1), second(formula2), second(formula3)))
	   	)
	   | LET declaration IN formula END (
	   		post_order := "LET \"let\", " ^ first(declaration) ^ ", IN \"in\", " ^ first(formula) ^ ", END \"end\", " ^ "formula -> LET declaration IN formula END"; 
	   		(!post_order, AST.LetExp(second(declaration), second(formula)))
	   	)
	   | formula PLUS formula (
	   		post_order := first(formula1) ^ ", " ^ "PLUS \"PLUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula PLUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Add(PLUS), second(formula1), second(formula2)))
	   	)
	   | formula MINUS formula (
	   		post_order := first(formula1) ^ ", " ^ "MINUS \"MINUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula MINUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Sub(MINUS), second(formula1), second(formula2)))
	   	)
	   | formula TIMES formula (
	   		post_order := first(formula1) ^ ", " ^ "TIMES \"TIMES\", " ^ first(formula2) ^ ", " ^ "formula -> formula TIMES formula" ; 
	   		(!post_order,AST.BinExp(AST.Mul(TIMES), second(formula1), second(formula2)))
	   	)
	   | NEGATE formula (
	   		post_order := "NEGATE \"NEGATE\", " ^ first(formula) ^ ", " ^ "formula -> NEGATE formula"; 
	   		(!post_order, AST.UnExp(AST.Neg(NEGATE), second(formula)))
	   	)
	   | formula LESSTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "LESSTHAN \"LESSTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula LESSTHAN formula" ; 
	   		(!post_order, AST.BinExp(AST.Lt(LESSTHAN), second(formula1), second(formula2)))
	   	)
	   | formula GREATERTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "GREATERTHAN \"GREATERTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula GREATERTHAN formula" ; 
	   		(!post_order, AST.BinExp(AST.Gt(GREATERTHAN), second(formula1), second(formula2)))
	   	)
	   | LPAREN formula formula RPAREN (
	   		post_order := "LPAREN \"(\", " ^ first(formula1) ^ ", " ^ first(formula2) ^ ", RPAREN \")\", formula -> LPAREN formula formula RPAREN";
	   		(!post_order, AST.AppExp(second(formula1), second(formula2)))
	   )
	   | FN LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FN \"fn\", LPAREN \"(\", ID \"" ^ ID ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\", " ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula";
			(!post_order, AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), ID, second(typ1), second(formula), second(typ2)))
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
	
		 
		 

