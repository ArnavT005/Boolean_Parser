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
							if(str = "TERM\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"program -> stmt_list\"\n"
							else if(str = "CONST\n" orelse str = "ID\n" orelse str = "RPAREN\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula BINOP formula\"\n"
							else if(str = "NOT\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> NOT formula\"\n"
							else if(str = "AND\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula AND formula\"\n"
							else if(str = "OR\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula OR formula\"\n"
							else if(str = "XOR\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula XOR formula\"\n"
							else if(str = "EQUALS\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula EQUALS formula\"\n"
							else if(str = "IMPLIES\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> formula IMPLIES formula\"\n"
							else if(str = "IF\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "THEN\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "ELSE\n") then error := "Syntax Error:" ^ Int.toString(first(pos)) ^ ":" ^ Int.toString(second(pos)) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n"
							else if(str = "LPAREN\n") then error := "Syntax Error: " ^ Int.toString(first(pos)) ^ ":" ^  Int.toString(second(pos)) ^ ":" ^ "\"formula -> LPAREN formula RPAREN\"\n"
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

	
		
		
	
	
