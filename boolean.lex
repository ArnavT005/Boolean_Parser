structure T = Tokens
type pos = int * int * int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1
val tokenNum = ref 1


val token_list = ref ([]: string list)

fun reverse(ls) = 
	let fun rev_iter([], ls) = ls
		|	rev_iter(x :: xs, ls) = rev_iter(xs, x :: ls)
	in
		rev_iter(ls, [])
	end
	
fun appendFile(str) = 
	let val outfile = TextIO.openAppend "lastToken"
	in(
		TextIO.output (outfile, str);
		TextIO.closeOut
	)
	end	
fun append list str1 str2 = 
		list := "\"" ^ str2 ^ "\"" :: str1 :: !list
		
fun secondaryPrint([]) = print ""
|	secondaryPrint(ls) =
		(print (", " ^ hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun printString([]) = print ""
|	printString(ls) = 
		(print (hd(ls) ^ " " ^ hd(tl(ls))); secondaryPrint(tl(tl(ls))));
		
fun error(x, y, str) = (TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(x) ^ ":" ^ Int.toString(y) ^ ":" ^ str ^ "\n"); OS.Process.exit(OS.Process.success));

fun colinc(x, str) = (x := !x + String.size str)

fun optionToString(SOME(str)) = str
|	optionToString(NONE) = ""

fun syntaxErrorIfAny() =
	let val infile = TextIO.openIn "Error";
	    val outfile = TextIO.openOut "Yes"
	in (
		if(TextIO.endOfStream infile) then print ""
		else 
			let val str = optionToString(TextIO.inputLine infile)
			in (
				print (str);
				OS.Process.exit(OS.Process.success)
			)
			end;
		TextIO.output (outfile, "Yes\n");
		TextIO.closeOut outfile
	)		
	end		
			

val eof = fn () => (print "["; printString(reverse(!token_list)); print "]\n"; syntaxErrorIfAny(); Tokens.EOF((!lineNum, !columnNum, !tokenNum), (!lineNum, !columnNum, !tokenNum)))	


%%

%header (functor booleanLexFun(structure Tokens: boolean_TOKENS));
alpha = [A-Za-z];
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
"IF"			 => (append token_list "IF" "IF"; colinc(columnNum, yytext); appendFile("IF\n"); tokenNum := !tokenNum + 1; T.IF((!lineNum, !columnNum - 2, !tokenNum - 1), (!lineNum, !columnNum - 2, !tokenNum - 1)));
"THEN"           => (append token_list "THEN" "THEN"; colinc(columnNum, yytext); appendFile("THEN\n"); tokenNum := !tokenNum + 1; T.THEN((!lineNum, !columnNum - 4, !tokenNum - 1), (!lineNum, !columnNum - 4, !tokenNum - 1)));
"ELSE"			 => (append token_list "ELSE" "ELSE"; colinc(columnNum, yytext); appendFile("ELSE\n"); tokenNum := !tokenNum + 1; T.ELSE((!lineNum, !columnNum - 4, !tokenNum - 1), (!lineNum, !columnNum - 4, !tokenNum - 1)));
"TRUE" | "FALSE" => (append token_list "CONST" yytext; colinc(columnNum, yytext); appendFile("CONST\n"); tokenNum := !tokenNum + 1; T.CONST(yytext, (!lineNum, !columnNum - String.size yytext, !tokenNum - 1), (!lineNum, !columnNum - String.size yytext, !tokenNum - 1)));
{alpha}+         => (append token_list "ID" yytext; colinc(columnNum, yytext); appendFile("ID\n"); tokenNum := !tokenNum + 1; T.ID(yytext, (!lineNum, !columnNum - String.size yytext, !tokenNum - 1), (!lineNum, !columnNum - String.size yytext, !tokenNum - 1)));
.                => (error(!lineNum, !columnNum, yytext); colinc(columnNum, yytext); lex());
