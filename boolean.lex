structure T = Tokens
type pos = int * int * int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1
val tokenNum = ref 1

val ifError = ref false


val token_list = ref ([]: string list)

fun reverse(ls) = 
	let fun rev_iter([], ls) = ls
		|	rev_iter(x :: xs, ls) = rev_iter(xs, x :: ls)
	in
		rev_iter(ls, [])
	end

fun Bvalue(str) =
	if(str = "TRUE") then true
	else false

fun Nvalue(str) =
	let val charList = String.explode str
	in
		let fun	listToNum([], num) = num
		|   listToNum(x :: xs, num) = listToNum(xs, 10 * num + Char.ord(x) - Char.ord(#"0"))
		in
			listToNum(charList, 0)
		end
	end
		
fun appendFile(str) = 
	let val outfile = TextIO.openAppend "lastToken"
	in(
		TextIO.output (outfile, str);
		TextIO.closeOut outfile
	)
	end
	
fun colinc(x, str) = (x := !x + String.size str)


fun optionToString(SOME(str)) = str
|	optionToString(NONE) = ""
	
fun append list str1 str2 = 
		list := "\"" ^ str2 ^ "\"" :: str1 :: !list
		
fun secondaryPrint([]) = print ""
|	secondaryPrint(ls) =
		(print (", " ^ hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun printString([]) = print ""
|	printString(ls) = 
		(print (hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun error(x, y, str) = (print("Unknown Token:" ^ Int.toString(x) ^ ":" ^ Int.toString(y) ^ ":" ^ str ^ "\n"); ifError := true);



fun syntaxErrorIfAny() =
	let val infile = TextIO.openIn "Error";
	    val outfile = TextIO.openOut "Yes"
	in (
		if(TextIO.endOfStream infile) then (TextIO.closeIn infile; print "")
		else 
			let val str = optionToString(TextIO.inputLine infile)
			in (
				print (str);
				TextIO.closeIn infile;
				OS.Process.exit(OS.Process.success)
			)
			end;
		TextIO.output (outfile, "Yes\n");
		TextIO.closeOut outfile
	)		
	end		
			

val eof = fn () => (
	if(!ifError = false) then
		(print "["; printString(reverse(!token_list)); print "]\n"; syntaxErrorIfAny(); Tokens.EOF((!lineNum, !columnNum, !tokenNum), (!lineNum, !columnNum, !tokenNum)))
	else
		OS.Process.exit(OS.Process.success))


%%

%header (functor booleanLexFun(structure Tokens: boolean_TOKENS));
alpha = [A-Za-z];
number = [0-9];
ws = [\ \t];

%%
\n               => (lineNum := !lineNum + 1; columnNum := 1; lex());
{ws}+            => (colinc(columnNum, yytext); lex());
";"      		 => (append token_list "TERM" ";"; colinc(columnNum, yytext); appendFile("TERM\n"); tokenNum := !tokenNum + 1; T.TERM((!lineNum, !columnNum - 1, !tokenNum - 1),(!lineNum, !columnNum - 1, !tokenNum - 1)));
"("				 => (append token_list "LPAREN" "("; colinc(columnNum, yytext); appendFile("LPAREN\n"); tokenNum := !tokenNum + 1; T.LPAREN((!lineNum, !columnNum - 1, !tokenNum - 1), (!lineNum, !columnNum - 1, !tokenNum - 1)));
")"				 => (append token_list "RPAREN" ")"; colinc(columnNum, yytext); appendFile("RPAREN\n"); tokenNum := !tokenNum + 1; T.RPAREN((!lineNum, !columnNum - 1, !tokenNum - 1), (!lineNum, !columnNum - 1, !tokenNum - 1)));
"NOT"			 => (append token_list "NOT" "NOT"; colinc(columnNum, yytext); appendFile("NOT\n"); tokenNum := !tokenNum + 1; T.NOT((!lineNum, !columnNum - 3, !tokenNum - 1), (!lineNum, !columnNum - 3, !tokenNum - 1)));
"AND"            => (append token_list "AND" "AND"; colinc(columnNum, yytext); appendFile("AND\n"); tokenNum := !tokenNum + 1; T.AND((!lineNum, !columnNum - 3, !tokenNum - 1), (!lineNum, !columnNum - 3, !tokenNum - 1)));
"OR"             => (append token_list "OR" "OR"; colinc(columnNum, yytext); appendFile("OR\n"); tokenNum := !tokenNum + 1; T.OR((!lineNum, !columnNum - 2, !tokenNum - 1), (!lineNum, !columnNum - 2, !tokenNum - 1)));
"XOR"			 => (append token_list "XOR" "XOR"; colinc(columnNum, yytext); appendFile("XOR\n"); tokenNum := !tokenNum + 1; T.XOR((!lineNum, !columnNum - 3, !tokenNum - 1), (!lineNum, !columnNum - 3, !tokenNum - 1)));
"EQUALS"		 => (append token_list "EQUALS" "EQUALS"; colinc(columnNum, yytext); appendFile("EQUALS\n"); tokenNum := !tokenNum + 1; T.EQUALS((!lineNum, !columnNum - 6, !tokenNum - 1), (!lineNum, !columnNum - 6, !tokenNum - 1)));
"IMPLIES"		 => (append token_list "IMPLIES" "IMPLIES"; colinc(columnNum, yytext); appendFile("IMPLIES\n"); tokenNum := !tokenNum + 1; T.IMPLIES((!lineNum, !columnNum - 7, !tokenNum - 1), (!lineNum, !columnNum - 7, !tokenNum - 1)));
"PLUS"			 => (append token_list "PLUS" "PLUS"; colinc(columnNum, yytext); appendFile("PLUS\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 4, !tokenNum - 1), (!lineNum, !columnNum - 4, !tokenNum - 1)));
"MINUS"			 => (append token_list "MINUS" "MINUS"; colinc(columnNum, yytext); appendFile("MINUS\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 5, !tokenNum - 1), (!lineNum, !columnNum - 5, !tokenNum - 1)));
"TIMES"			 => (append token_list "TIMES" "TIMES"; colinc(columnNum, yytext); appendFile("TIMES\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 5, !tokenNum - 1), (!lineNum, !columnNum - 5, !tokenNum - 1)));
"NEGATE"		 => (append token_list "NEGATE" "NEGATE"; colinc(columnNum, yytext); appendFile("NEGATE\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 6, !tokenNum - 1), (!lineNum, !columnNum - 6, !tokenNum - 1)));
"LESSTHAN"		 => (append token_list "LESSTHAN" "LESSTHAN"; colinc(columnNum, yytext); appendFile("LESSTHAN\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 8, !tokenNum - 1), (!lineNum, !columnNum - 8, !tokenNum - 1)));
"GREATERTHAN"	 => (append token_list "GREATERTHAN" "GREATERTHAN"; colinc(columnNum, yytext); appendFile("GREATERTHAN\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 11, !tokenNum - 1), (!lineNum, !columnNum - 11, !tokenNum - 1)));
"if"			 => (append token_list "if" "if"; colinc(columnNum, yytext); appendFile("if\n"); tokenNum := !tokenNum + 1; T.if((!lineNum, !columnNum - 2, !tokenNum - 1), (!lineNum, !columnNum - 2, !tokenNum - 1)));
"then"           => (append token_list "then" "then"; colinc(columnNum, yytext); appendFile("then\n"); tokenNum := !tokenNum + 1; T.then((!lineNum, !columnNum - 4, !tokenNum - 1), (!lineNum, !columnNum - 4, !tokenNum - 1)));
"else"			 => (append token_list "else" "else"; colinc(columnNum, yytext); appendFile("else\n"); tokenNum := !tokenNum + 1; T.else((!lineNum, !columnNum - 4, !tokenNum - 1), (!lineNum, !columnNum - 4, !tokenNum - 1)));
"fi"			 => (append token_list "fi" "fi"; colinc(columnNum, yytext); appendFile("fi\n"); tokenNum := !tokenNum + 1; T.fi((!lineNum, !columnNum - 2, !tokenNum - 1), (!lineNum, !columnNum - 2, !tokenNum - 1)));
"let"			 => (append token_list "let" "let"; colinc(columnNum, yytext); appendFile("let\n"); tokenNum := !tokenNum + 1; T.let((!lineNum, !columnNum - 3, !tokenNum - 1), (!lineNum, !columnNum - 3, !tokenNum - 1)));
"in"			 => (append token_list "in" "in"; colinc(columnNum, yytext); appendFile("in\n"); tokenNum := !tokenNum + 1; T.in((!lineNum, !columnNum - 2, !tokenNum - 1), (!lineNum, !columnNum - 2, !tokenNum - 1)));
"end"			 => (append token_list "end" "end"; colinc(columnNum, yytext); appendFile("end\n"); tokenNum := !tokenNum + 1; T.end((!lineNum, !columnNum - 3, !tokenNum - 1), (!lineNum, !columnNum - 3, !tokenNum - 1)));
"TRUE" | "FALSE" => (append token_list "CONST_Bool" yytext; colinc(columnNum, yytext); appendFile("CONST_Bool\n"); tokenNum := !tokenNum + 1; T.CONST_Bool(Bvalue(yytext), (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1), (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1)));
{number}+		 => (append token_list "CONST_Int" yytext; colinc(columnNum, yytext); appendFile("CONST_Int\n"); tokenNum := !tokenNum + 1; T.CONST_Int(Nvalue(yytext), (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1), (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1)));
{alpha}+         => (append token_list "ID" yytext; colinc(columnNum, yytext); appendFile("ID\n"); tokenNum := !tokenNum + 1; T.ID(yytext, (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1), (!lineNum, !columnNum - String.size(yytext), !tokenNum - 1)));
.                => (error(!lineNum, !columnNum, yytext); colinc(columnNum, yytext); lex());
