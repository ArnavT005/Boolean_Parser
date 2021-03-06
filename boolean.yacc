val post_order = ref ""

fun IntStarInt(a, b) = Int.toString(a) ^ ":" ^ Int.toString(b) ^ ":";

fun StringStarIntStarInt(str, a, b) = Int.toString(a) ^ ":" ^ Int.toString(b) ^ ":";

fun ToString(str, a: int, b: int) = str;

%%

%name boolean

%term EOF | TERM | CONST of string | NOT | AND | OR  | XOR  |
	  EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | ID of string

%nonterm program of unit | stmt_list of string | statement of string | formula of string

%pos int

%eop EOF
%noshift EOF

%right THEN ELSE
%right IF
%right IMPLIES
%left EQUALS OR XOR AND
%right NOT

%start program

%verbose

%%
program: stmt_list (post_order := stmt_list1 ^ ", " ^ "program -> stmt_list\n"; print(!post_order))
stmt_list: statement (post_order := statement1 ^ ", " ^ "stmt_list -> statement"; !post_order)
		 | stmt_list statement (post_order := stmt_list1 ^ ", " ^ statement1 ^ ", " ^ "stmt_list -> stmt_list statement"; !post_order)
statement: formula TERM (post_order := formula ^ ", " ^ "TERM \";\", " ^ "statement -> formula TERM"; !post_order)
formula: CONST (post_order := "CONST \"" ^ CONST ^ "\", " ^ "formula -> CONST"; !post_order)
	   | ID (post_order := "ID \"" ^ ID ^ "\", " ^ "formula -> ID"; !post_order)
	   | LPAREN formula RPAREN (post_order := "LPAREN \"(\", " ^ formula1 ^ ", " ^ "RPAREN \")\", " ^ "formula -> LPAREN formula RPAREN"; !post_order)
	   | NOT formula (post_order := "NOT \"NOT\", " ^ formula1 ^ ", " ^ "formula -> NOT formula"; !post_order)
	   | formula IMPLIES formula (post_order := formula1 ^ ", " ^ "IMPLIES \"IMPLIES\", " ^ formula2 ^ ", " ^ "formula -> formula IMPLIES formula"; !post_order)
	   | formula AND formula (post_order := formula1 ^ ", " ^ "AND \"AND\", " ^ formula2 ^ ", " ^ "formula -> formula AND formula" ; !post_order)
	   | formula OR formula (post_order := formula1 ^ ", " ^ "OR \"OR\", " ^ formula2 ^ ", " ^ "formula -> formula OR formula"; !post_order)
	   | formula XOR formula (post_order := formula1 ^ ", " ^ "XOR \"XOR\", " ^ formula2 ^ ", " ^ "formula -> formula XOR formula"; !post_order)
	   | formula EQUALS formula (post_order := formula1 ^ ", " ^ "EQUALS \"EQUALS\", " ^ formula2 ^ ", " ^ "formula -> formula EQUALS formula"; !post_order)
	   | IF formula THEN formula ELSE formula (post_order := "IF \"IF\", " ^ formula1 ^ ", " ^ "THEN \"THEN\", " ^ formula2 ^ ", " ^ "ELSE \"ELSE\", " ^ formula3 ^ ", " ^ "formula -> IF formula THEN formula ELSE formula"; !post_order)



		   
				 


		 
		 
