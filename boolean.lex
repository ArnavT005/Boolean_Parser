structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1

val outfile = TextIO.openOut "lastToken"

val token_list = ref ([]: string list)

fun reverse(ls) = 
	let fun rev_iter([], ls) = ls
		|	rev_iter(x :: xs, ls) = rev_iter(xs, x :: ls)
	in
		rev_iter(ls, [])
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
	let val infile = TextIO.openIn "Error"
	in
		if(TextIO.endOfStream infile) then print ""
		else 
			let val str = optionToString(TextIO.inputLine infile)
			in (
				print (str ^ "\n");
				OS.Process.exit(OS.Process.success)
			)
			end
	end		
			

val eof = fn () => (print "["; printString(reverse(!token_list)); print "]\n"; syntaxErrorIfAny(); Tokens.EOF(!lineNum, !columnNum))	


%%

%header (functor booleanLexFun(structure Tokens: boolean_TOKENS));
alpha = [A-Za-z];
ws = [\ \t];

%%
\n               => (lineNum := !lineNum + 1; columnNum := 1; lex());
{ws}+            => (colinc(columnNum, yytext); lex());
";"      		 => (append token_list "TERM" ";"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "TERM"); T.TERM(!lineNum, !columnNum - 1));
"("				 => (append token_list "LPAREN" "("; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "LPAREN"); T.LPAREN(!lineNum, !columnNum - 1));
")"				 => (append token_list "RPAREN" ")"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "RPAREN"); T.RPAREN(!lineNum, !columnNum - 1));
"NOT"			 => (append token_list "NOT" "NOT"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "NOT"); T.NOT(!lineNum, !columnNum - 3));
"AND"            => (append token_list "AND" "AND"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "AND"); T.AND(!lineNum, !columnNum - 3));
"OR"             => (append token_list "OR" "OR"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "OR"); T.OR(!lineNum, !columnNum - 2));
"XOR"			 => (append token_list "XOR" "XOR"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "XOR"); T.XOR(!lineNum, !columnNum - 3));
"EQUALS"		 => (append token_list "EQUALS" "EQUALS"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "EQUALS"); T.EQUALS(!lineNum, !columnNum - 6));
"IMPLIES"		 => (append token_list "IMPLIES" "IMPLIES"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "IMPLIES"); T.IMPLIES(!lineNum, !columnNum - 7));
"IF"			 => (append token_list "IF" "IF"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "IF"); T.IF(!lineNum, !columnNum - 2));
"THEN"           => (append token_list "THEN" "THEN"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "THEN"); T.THEN(!lineNum, !columnNum - 4));
"ELSE"			 => (append token_list "ELSE" "ELSE"; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "ELSE"); T.ELSE(!lineNum, !columnNum - 4));
"TRUE" | "FALSE" => (append token_list "CONST" yytext; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "CONST"); T.CONST(yytext, !lineNum, !columnNum - String.size yytext));
{alpha}+         => (append token_list "ID" yytext; colinc(columnNum, yytext); TextIO.output (TextIO.openOut "lastToken", "ID"); T.ID(yytext, !lineNum, !columnNum - String.size yytext));
.                => (error(!lineNum, !columnNum, yytext); colinc(columnNum, yytext); lex());
