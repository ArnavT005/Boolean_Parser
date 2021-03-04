structure booleanLrVals = booleanLrValsFun(structure Token = LrParser.Token)
structure booleanLex = booleanLexFun(structure Tokens = booleanLrVals.Tokens);
structure booleanParser =
	Join(structure LrParser = LrParser
		 structure ParserData = booleanLrVals.ParserData
		 structure Lex = booleanLex)
		 
fun invoke lexstream =
	let fun print_error(x, y, str) =
			TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(x) ^ ":" ^ Int.toString(y) ^ ":" ^ str ^ "\n")
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
	handle IO => print("File cannot be opened. Terminating!\n")

fun lexerToParser lexer = 
	let val dummyEOF = booleanLrVals.Tokens.EOF(0, 0);
		val (result, lexer) = invoke lexer;
		val (nextToken, lexer) = booleanParser.Stream.get lexer
	in
		if(booleanParser.sameToken(nextToken, dummyEOF) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed Tokens.\n"); result)
	end	
	
val fileName = CommandLine.arguments();
val str = fileToString (hd(fileName));
val lexer = stringToLexer str;
lexerToParser lexer;
	
		
		
	
	