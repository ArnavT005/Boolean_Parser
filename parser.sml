structure booleanLrVals = booleanLrValsFun(structure Token = LrParser.Token)
structure booleanLex = booleanLexFun(structure Tokens = booleanLrVals.Tokens);
structure booleanParser =
	Join(structure LrParser = LrParser
		 structure ParserData = booleanLrVals.ParserData
		 structure Lex = booleanLex)
		 
fun invoke lexstream =
	let fun print_error(str, pos1) =
			TextIO.output(TextIO.stdOut, "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ str ^ "\n")
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
	let val dummyEOF = booleanLrVals.Tokens.EOF(0, 0);
		val (result, lexer) = invoke lexer;
		val (nextToken, lexer) = booleanParser.Stream.get lexer
	in
		if(booleanParser.sameToken(nextToken, dummyEOF)) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed Tokens.\n"); result)
	end	
	
(*val fileName = CommandLine.arguments();
val str = fileToString (hd(fileName));*)
val parseString = lexerToParser o stringToLexer 
(*val lexer = stringToLexer str;
lexerToParser lexer;*)
	
		
		
	
	
