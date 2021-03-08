structure booleanLrVals = booleanLrValsFun(structure Token = LrParser.Token)
structure booleanLex = booleanLexFun(structure Tokens = booleanLrVals.Tokens);
structure booleanParser =
	Join(structure LrParser = LrParser
		 structure ParserData = booleanLrVals.ParserData
		 structure Lex = booleanLex)

val error = ref ""
val ifError = ref false

fun optionToString(SOME(str)) = str
|	optionToString(NONE) = ""
		 
		 
fun invoke lexstream =
	let fun print_error(str, pos1, pos2) =
			let val infile = TextIO.openIn "lastToken";
				val str = optionToString(TextIO.inputLine infile);
				val outfile = TextIO.openOut "Error"
			in (
				if(not (!ifError)) then
					if(str = "TERM") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"program -> stmt_list\"\n";
					else if(str = "CONST" orelse str = "ID" orelse str = "RPAREN") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula BINOP formula\"\n";
					else if(str = "NOT") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> NOT formula\"\n";
					else if(str = "AND") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula AND formula\"\n";
					else if(str = "OR") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula OR formula\"\n";
					else if(str = "XOR") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula XOR formula\"\n";
					else if(str = "EQUALS") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula EQUALS formula\"\n";
					else if(str = "IMPLIES") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> formula IMPLIES formula\"\n";
					else if(str = "IF") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n";
					else if(str = "THEN") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n";
					else if(str = "ELSE") then error := "Syntax Error:" ^ Int.toString(pos1) ^ ":" ^ Int.toString(pos2) ^ ":" ^ "\"formula -> IF formula THEN formula ELSE formula\"\n";
					else error := "Syntax Error: " ^ Int.toString(pos1) ^ ":" ^  Int.toString(pos2) ^ ":" ^ "\"formula -> LPAREN formula RPAREN\"\n";
				else error := !error;
				TextIO.output (outfile, !error)
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
	let val dummyEOF = booleanLrVals.Tokens.EOF(0, 0);
		val (result, lexer) = invoke lexer;
		val (nextToken, lexer) = booleanParser.Stream.get lexer
	in
		if(booleanParser.sameToken(nextToken, dummyEOF)) then result
		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed Tokens.\n"); result)
	end	
	
fun recurseLex str = 
	let val lexer = BoolLex.makeLexer(fn _ => str))
		val nextToken = ref lexer();
		val dummyEOF = EOF(0, 0)
	in
		while(!nextToken <> dummyEOF) do
		( 
			nextToken := lexer();
		)
	end	
(*
val fileName = CommandLine.arguments();
val str = fileToString (hd(fileName));*)
val parseString = lexerToParser o stringToLexer 
(*val lexer = stringToLexer str;
lexerToParser lexer;*)
	
		
		
	
	
