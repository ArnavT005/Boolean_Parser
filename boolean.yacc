
val post_order = ref ""
val statNum = ref 0
val funcValEnv = ref ([]: AST.valEnvironment)
val funcTypEnv = ref ([]: AST.typEnvironment)
fun first(f, _) = f
fun second(_, s) = s



fun valToString(AST.IntVal (i)) = "Value of expression = " ^ Int.toString(i) ^ ": INT"
|   valToString(AST.BoolVal (b)) = 
		if(b) then "Value of expression = true: BOOL"
		else "Value of expression = false: BOOL"
|   valToString(_) = "Value of expression CAN NOT be DETERMINED"


fun boolToString(b) = 
	if(b) then "TRUE"
	else "FALSE"


%%

%name boolean

%term EOF | TERM | CONST_Bool of bool | CONST_Int of int | NOT | AND | OR  | XOR  |
	  EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of AST.id |
	  PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | FI | LET | IN | END | EQ |
	  COLON | FN | FUN | ARROW | DEF | INT | BOOL

(*Add new operator tokens and other keywords*)

%nonterm program of unit | stmt_list of string * string | statement of string * string | 
		 formula of string * AST.exp | declaration of string * AST.decl | typ of string * AST.Generic |
		 lambda of string * AST.exp

%pos int * int * int

%eop EOF
%noshift EOF

(*%right IF THEN ELSE*)
%right DEF
%right IMPLIES
%left EQUALS OR XOR AND
%nonassoc LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NOT NEGATE ARROW

%start program

%verbose

%%


program: stmt_list (
			post_order := first(stmt_list) ^ ", " ^ "program -> stmt_list\n"; 
			print(!post_order); 
			print("Evaluated value for each statement:\n" ^ second(stmt_list) ^ "\n")
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
			  let val v = AST.evalExp(second(formula), !funcValEnv)
			  in 
			  	(!post_order, "Statement:" ^ Int.toString(!statNum) ^ ":" ^ valToString(v))
			  end		
		 )
		| FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FUN \"fun\", ID \"" ^ ID1 ^ "\", LPAREN \"(\", ID \"" ^ ID2 ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\"" ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula";
			funcValEnv := (ID1, AST.Lambda(AST.ARROW(second(typ1), second(typ2)), ID2, second(typ1), second(formula), second(typ2))) :: !funcValEnv;
			(!post_order, "")
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
	   		(!post_order, AST.UnExp(AST.Not, second(formula)))
	   	)
	   | formula IMPLIES formula (
	   		post_order := first(formula1) ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ first(formula2) ^ ", " ^ "formula -> formula IMPLIES formula"; 
	   		(!post_order,AST.BinExp(AST.Implies, second(formula1), second(formula2)))
	   	)
	   | formula AND formula (
	   		post_order := first(formula1) ^ ", " ^ "AND \"AND\", " ^ first(formula2) ^ ", " ^ "formula -> formula AND formula" ; 
	   		(!post_order,AST.BinExp(AST.And, second(formula1), second(formula2)))
	   	)
	   | formula OR formula (
	   		post_order := first(formula1) ^ ", " ^ "OR \"OR\", " ^ first(formula2) ^ ", " ^ "formula -> formula OR formula"; 
	   		(!post_order,AST.BinExp(AST.Or, second(formula1), second(formula2)))
	   	)
	   | formula XOR formula (
	   		post_order := first(formula1) ^ ", " ^ "XOR \"XOR\", " ^ first(formula2) ^ ", " ^ "formula -> formula XOR formula"; 
	   		(!post_order,AST.BinExp(AST.Xor, second(formula1), second(formula2)))
	   	)
	   | formula EQUALS formula (
	   		post_order := first(formula1) ^ ", " ^ "EQUALS \"EQUALS\", " ^ first(formula2) ^ ", " ^ "formula -> formula EQUALS formula"; 
	   		(!post_order,AST.BinExp(AST.Eq, second(formula1), second(formula2)))
	   	)
	   | IF formula THEN formula ELSE formula FI (
	   		post_order := "IF \"if\", " ^ first(formula1) ^ ", " ^ "THEN \"then\", " ^ first(formula2) ^ ", " ^ "ELSE \"else\", " ^ first(formula3) ^ ", FI \"fi\", " ^ "formula -> IF formula THEN formula ELSE formula"; 
	   		(!post_order,AST.ConExp(second(formula1), second(formula2), second(formula3)))
	   	)
	   | LET declaration IN formula END (
	   		post_order := "LET \"let\", " ^ first(declaration) ^ ", IN \"in\", " ^ first(formula) ^ ", END \"end\", " ^ "formula -> LET declaration IN formula END"; 
	   		(!post_order, AST.LetExp(second(declaration), second(formula)))
	   	)
	   | formula PLUS formula (
	   		post_order := first(formula1) ^ ", " ^ "PLUS \"PLUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula PLUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Add, second(formula1), second(formula2)))
	   	)
	   | formula MINUS formula (
	   		post_order := first(formula1) ^ ", " ^ "MINUS \"MINUS\", " ^ first(formula2) ^ ", " ^ "formula -> formula MINUS formula" ; 
	   		(!post_order,AST.BinExp(AST.Sub, second(formula1), second(formula2)))
	   	)
	   | formula TIMES formula (
	   		post_order := first(formula1) ^ ", " ^ "TIMES \"TIMES\", " ^ first(formula2) ^ ", " ^ "formula -> formula TIMES formula" ; 
	   		(!post_order,AST.BinExp(AST.Mul, second(formula1), second(formula2)))
	   	)
	   | NEGATE formula (
	   		post_order := "NEGATE \"NEGATE\", " ^ first(formula) ^ ", " ^ "formula -> NEGATE formula"; 
	   		(!post_order, AST.UnExp(AST.Neg, second(formula)))
	   	)
	   | formula LESSTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "LESSTHAN \"LESSTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula LESSTHAN formula" ; 
	   		(!post_order, AST.BinExp(AST.Lt, second(formula1), second(formula2)))
	   	)
	   | formula GREATERTHAN formula (
	   		post_order := first(formula1) ^ ", " ^ "GREATERTHAN \"GREATERTHAN\", " ^ first(formula2) ^ ", " ^ "formula -> formula GREATERTHAN formula" ; 
	   		(!post_order,AST.BinExp(AST.Gt, second(formula1), second(formula2)))
	   	)
	   | LPAREN ID formula RPAREN (
	   		post_order := "LPAREN \"(\", ID \"" ^ ID ^ "\", " ^ first(formula) ^ ", RPAREN \")\", formula -> LPAREN ID formula RPAREN";
	   		(!post_order, AST.AppExp(AST.VarExp(ID), second(formula)))
	   )
	   | LPAREN lambda formula RPAREN (
	   		post_order := "LPAREN \"(\", " ^ first(lambda) ^ ", " ^ first(formula) ^ ", RPAREN \")\", formula -> LPAREN lambda formula RPAREN";
	   		(!post_order, AST.AppExp(second(lambda), second(formula)))
	   )
	   | lambda (
	   		post_order := first(lambda) ^ ", formula -> lambda";
	   		(!post_order, second(lambda))
	   )
				 

lambda: FN LPAREN ID COLON typ RPAREN COLON typ DEF formula (
			post_order := "FN \"fn\", LPAREN \"(\", ID \"" ^ ID ^ "\", COLON \":\", " ^ first(typ1) ^ ", RPAREN \")\", COLON \":\"" ^ first(typ2) ^ ", DEF \"=>\", " ^ first(formula) ^ ", function -> FN LPAREN ID COLON type RPAREN COLON typ DEF formula";
			(!post_order, AST.AbsExp(AST.ARROW(second(typ1), second(typ2)), ID, second(typ1), second(formula), second(typ2)))
		) 

typ:	INT (post_order := "INT \"int\", typ -> INT"; (!post_order, AST.INT))
	|   BOOL (post_order := "BOOL \"bool\", typ -> BOOL"; (!post_order, AST.BOOL))
	|   typ ARROW typ (
		post_order := first(typ1) ^ ", ARROW \"->\", " ^ first(typ2) ^ ", typ -> typ ARROW typ";
		(!post_order, AST.ARROW(second(typ1), second(typ2)))
	) 
	
		 
		 
