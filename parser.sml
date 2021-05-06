structure booleanLrVals = booleanLrValsFun(structure Token = LrParser.Token)
structure booleanLex = booleanLexFun(structure Tokens = booleanLrVals.Tokens);
structure booleanParser =
	Join(structure LrParser = LrParser
		 structure ParserData = booleanLrVals.ParserData
		 structure Lex = booleanLex)


val error = ref ""
val ifError = ref false

fun maybe(str) =
	let val infile = TextIO.openIn "Yes"
	in
		if(TextIO.endOfStream infile) then (TextIO.closeIn infile)
		else (print(str); TextIO.closeIn infile; OS.Process.exit(OS.Process.success))
	end	

fun optionToString(SOME(str)) = str
|	optionToString(NONE) = ""
		 
fun first(n1, _, _) = n1
fun second(_, n2, _) = n2
fun third(_, _, n3) = n3

fun readToken(infile, num, str) = 
	if(num = 0) then (TextIO.closeIn infile; str)
	else 
		let val s = optionToString(TextIO.inputLine infile)
		in
			readToken(infile, num - 1, s)
		end
		
		 
fun invoke lexstream =
	let fun print_error(str, pos, _) =
			let val infile = TextIO.openIn "lastToken";
				val outfile = TextIO.openOut "Error"
			in (
				if(TextIO.endOfStream infile) then (error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"program -> stmt_list\"\n"; ifError := true; TextIO.closeIn infile)
				else
					let val num = third(pos);
						val str = readToken(infile, num - 1, "")
					in (	
						if(not (!ifError)) then (
							if(str = "TERM\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"stmt_list -> stmt_list TERM statement\"\n"
							else if(str = "ID\n" orelse str = "CONST_Bool" orelse str = "CONST_Int\n") then 
								let val num1 = num - 2
								in
									if(num1 >= 1) then
										let val infile1 = TextIO.openIn "lastToken";
											val str1 = readToken(infile1, num1, "");
										in
											if(str1 = "FUN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
											else if(str1 = "LPAREN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula, statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
											else if(str1 = "LET\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"declaration -> ID EQ formula\"\n"
											else error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula BINOP formula, stmt_list -> stmt_list TERM statement\"\n"

										end
									else
										error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula BINOP formula, stmt_list -> stmt_list TERM statement\"\n"
								end
							else if(str = "NOT\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> NOT formula\"\n"
							else if(str = "AND\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula AND formula\"\n"
							else if(str = "OR\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula OR formula\"\n"
							else if(str = "XOR\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula XOR formula\"\n"
							else if(str = "EQUALS\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula EQUALS formula\"\n"
							else if(str = "IMPLIES\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula IMPLIES formula\"\n"
							else if(str = "NEGATE\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> NEGATE formula\"\n"
							else if(str = "PLUS\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula PLUS formula\"\n"
							else if(str = "MINUS\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula MINUS formula\"\n"
							else if(str = "TIMES\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula TIMES formula\"\n"
							else if(str = "LESSTHAN\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula LESSTHAN formula\"\n"
							else if(str = "GREATERTHAN\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula GREATERTHAN formula\"\n"
							else if(str = "IF\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "THEN\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "ELSE\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "FI\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"stmt_list -> stmt_list TERM statement\"\n"
							else if(str = "LET\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> LET declaration IN formula END\"\n"
							else if(str = "IN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> LET declaration IN formula END\"\n"
							else if(str = "END\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"stmt_list -> stmt_list TERM statement\"\n"
							else if(str = "EQ\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"declaration -> ID EQ formula\"\n"
							else if(str = "ARROW\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"typ -> typ ARROW typ\"\n"
							else if(str = "FN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
							else if(str = "FUN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
							else if(str = "COLON\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula, statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
							else if(str = "DEF\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula, statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
							else if(str = "INT\n" orelse str = "BOOL\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"typ -> LPAREN typ RPAREN, typ -> typ ARROW typ\"\n"
							else if(str = "LPAREN\n") then 
								let val num1 = num - 2
								in
									if(num1 >= 1) then
										let val infile1 = TextIO.openIn "lastToken";
											val str1 = readToken(infile1, num1, "");
										in
											if(str1 = "FN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
											else if(str1 = "ID\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
											else if(str1 = "COLON\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"typ -> LPAREN typ RPAREN\"\n"
											else error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> LPAREN formula RPAREN, formula -> LPAREN formula formula RPAREN, typ -> LPAREN typ RPAREN\"\n"
										end
									else
										error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> LPAREN formula RPAREN, formula -> LPAREN formula formula RPAREN\"\n"
								end
							else if(str = "RPAREN\n") then 
								let val num1 = num - 2
								in
									if(num1 >= 1) then
										let val infile1 = TextIO.openIn "lastToken";
											val str1 = readToken(infile1, num1, "")
										in
											if(str1 = "INT\n" orelse str1 = "BOOL\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> FN LPAREN ID COLON typ RPAREN COLON typ DEF formula, statement -> FUN ID LPAREN ID COLON typ RPAREN COLON typ DEF formula\"\n"
											else error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"stmt_list -> stmt_list TERM statement\"\n"
										end
									else
										error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"stmt_list -> stmt_list TERM statement\"\n"
								end	
							else error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"program -> stmt_list\"\n";
							ifError := true
						)	
						else error := !error
					)
					end;
				maybe(!error);
				TextIO.output (outfile, !error);
				TextIO.closeOut outfile
			)
			end				
	in
		booleanParser.parse(0, lexstream, print_error, ())
	end
	
fun stringToLexer str =
	let val done = ref false;
		val lexer = booleanParser.makeLexer(fn _ => if(!done) then "" else (done := true; str))
	in
		lexer
	end
	
fun fileToString fileName = 
	let val instream = TextIO.openIn fileName
	in
		TextIO.input instream
	end

fun lexerToParser lexer = 
	let val dummyEOF = booleanLrVals.Tokens.EOF((0, 0, 0), (0, 0, 0));
		val (result, lexer) = invoke lexer;
		val (nextToken, lexer) = booleanParser.Stream.get lexer
	in
		if(booleanParser.sameToken(nextToken, dummyEOF)) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed Tokens.\n"); result)
	end	

val outfile1 = TextIO.openOut "lastToken";
TextIO.closeOut(outfile1);
val outfile2 = TextIO.openOut "Error";
TextIO.closeOut(outfile2);	
val outfile3 = TextIO.openOut "Yes";
TextIO.closeOut(outfile3);	


val fileName = CommandLine.arguments();
val str = fileToString (hd(fileName));
val parseString = lexerToParser o stringToLexer; 

parseString str;

	
		
		
	
	
