val pre_order = ref ""

%%

%name boolean

%term EOF | TERM | CONST of string | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE |
	  LPAREN | RPAREN | ID of string

%nonterm program of string | stmt_list of string | statement of string | formula of string

%pos int

%eop EOF
%noshift EOF

%right IF THEN ELSE
%right IMPLIES
%left EQUALS OR XOR AND
%right NOT

%start program

%verbose

%%
program: stmt_list (pre_order := "program -> stmt_list, " ^ stmt_list1; !pre_order)
stmt_list: statement (pre_order := "stmt_list -> statement, " ^ statement1; !pre_order)
		 | stmt_list statement (pre_order := "stmt_list -> stmt_list statement, " ^ stmt_list1 ^ ", " ^ statement1; !pre_order)
statement: formula TERM (pre_order := "statement -> formula TERM, " ^ formula ^ ", " ^ "TERM \";\""; !pre_order)
		 | TERM (pre_order := "statement -> TERM, " ^ "TERM \";\""; !pre_order)
formula: CONST (pre_order := "formula -> CONST, " ^ "CONST \"" ^ CONST ^ "\""; !pre_order)
	   | ID (pre_order := "formula -> ID, " ^ "ID \"" ^ ID "\""; !pre_order)
	   | LPAREN formula RPAREN (pre_order := "formula -> LPAREN formula RPAREN, " ^ "LPAREN \"(\", " ^ formula1 ^ ", " ^ "RPAREN \")\""; !pre_order)
	   | NOT formula (pre_order := "formula -> NOT formula, " ^ "NOT \"NOT\", " ^ formula1; !pre_order)
	   | formula IMPLIES formula (pre_order := "formula -> formula IMPLIES formula, " ^ formula1 ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ formula2; !pre_order)
	   | formula AND formula (pre_order := "formula -> formula AND formula, " ^ formula1 ^ ", " ^ "AND \"AND\", " ^ formula2; !pre_order)
	   | formula OR formula (pre_order := "formula -> formula OR formula, " ^ formula1 ^ ", " ^ "OR \"OR\", " ^ formula2; !pre_order)
	   | formula XOR formula (pre_order := "formula -> formula XOR formula, " ^ formula1 ^ ", " ^ "XOR \"XOR\", " ^ formula2; !pre_order)
	   | formula EQUALS formula (pre_order := "formula -> formula EQUALS formula, " ^ formula1 ^ ", " ^ "EQUALS \"EQUALS\", " ^ formula2; !pre_order)
	   | IF formula THEN formula ELSE formula (pre_order := "formula -> IF formula THEN formula ELSE formula, " ^ "IF \"IF\", " ^ formula1 ^ ", " ^ "THEN \"THEN\", " ^ formula2 ^ ", " ^ "ELSE \"ELSE\", " formula3; !pre_order)



		   
				 


		 
		 
