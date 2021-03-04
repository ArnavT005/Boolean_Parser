
%%

%name boolean

%term EOF | TERM | CONST | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE |
	  LPAREN | RPAREN | ID of string

%nonterm code_file | program | statement | conditional_formula | implicit_formula | binary_formula |
         not_formula | operand

%pos int
%eop EOF
%noshift EOF
%right THEN ELSE
%right IF
%right IMPLIES
%left EQUALS OR XOR AND
%right NOT
%start code_file

%verbose

%%
code_file: program ()
program: statement ()
		|program statement ()
statement: conditional_formula TERM ()
		|  TERM ()
conditional_formula: IF conditional_formula THEN conditional_formula ELSE conditional_formula ()
				   | implicit_formula ()
implicit_formula: binary_formula IMPLIES implicit_formula ()
				| binary_formula ()
binary_formula: binary_formula AND not_formula ()
			  | binary_formula OR not_formula ()
			  | binary_formula XOR not_formula ()
			  | binary_formula EQUALS not_formula ()
              | not_formula ()
not_formula: NOT not_formula ()
		   | operand ()
operand: CONST () | ID () | LPAREN conditional_formula RPAREN ()


		   
				 


		 
		 
