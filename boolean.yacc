
%%

%name boolean

%term EOF | TERM | CONST | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE |
	  LPAREN | RPAREN | ID of string

%nonterm program | stmt_list | statement | formula

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
program: stmt_list ()
stmt_list: statement ()
		 | stmt_list statement ()
statement: formula TERM ()
		 | TERM ()
formula: CONST ()
	   | ID ()
	   | LPAREN formula RPAREN ()
	   | NOT formula ()
	   | formula IMPLIES formula ()
	   | formula AND formula ()
	   | formula OR formula ()
	   | formula XOR formula ()
	   | formula EQUALS formula ()
	   | IF formula THEN formula ELSE formula ()



		   
				 


		 
		 
