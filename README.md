Assignment-3: Instructions for execution

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

Main code files used by me:
- boolean.lex
- boolean.yacc
- parser.sml
- ast.sml
- makefile

parser.mlb is the loader file and will form an executable named parser when compiled.

NOTE: mlton (along with mllex and mlyacc) should be available for proper execution
	  of makefile.

Command to form executable:
make
(or make all)

Instructions to run the executable:
`./parser <file_name>`

NOTE: My program creates three auxilliary files named "Yes", "Error" and "lastToken". 
	  These files are used for reporting syntax error raised by the parser.

CLEAN command:
Use `make clean` to clear all the extra files (like .sig, lex.sml, aux files etc.)

-------------------------------------------------------------------------------
