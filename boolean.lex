structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1

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

val eof = fn () => (print "["; printString(reverse(!token_list)); print "]\n"; Tokens.EOF(!lineNum, !columnNum, 0, 0))	


%%

%header (functor booleanLexFun(structure Tokens: boolean_TOKENS));
alpha = [A-Za-z];
ws = [\ \t];

%%
\n               => (lineNum := !lineNum + 1; columnNum := 1; lex());
{ws}+            => (colinc(columnNum, yytext); lex());
";"      		 => (append token_list "TERM" ";"; colinc(columnNum, yytext); T.TERM(!lineNum, !columnNum - 1, yypos, yypos + String.size yytext));
"("				 => (append token_list "LPAREN" "("; colinc(columnNum, yytext); T.LPAREN(!lineNum, !columnNum - 1, yypos, yypos + String.size yytext));
")"				 => (append token_list "RPAREN" ")"; colinc(columnNum, yytext); T.RPAREN(!lineNum, !columnNum - 1, yypos, yypos + String.size yytext));
"NOT"			 => (append token_list "NOT" "NOT"; colinc(columnNum, yytext); T.NOT(!lineNum, !columnNum - 3, yypos, yypos + String.size yytext));
"AND"            => (append token_list "AND" "AND"; colinc(columnNum, yytext); T.AND(!lineNum, !columnNum - 3, yypos, yypos + String.size yytext));
"OR"             => (append token_list "OR" "OR"; colinc(columnNum, yytext); T.OR(!lineNum, !columnNum - 2, yypos, yypos + String.size yytext));
"XOR"			 => (append token_list "XOR" "XOR"; colinc(columnNum, yytext); T.XOR(!lineNum, !columnNum - 3, yypos, yypos + String.size yytext));
"EQUALS"		 => (append token_list "EQUALS" "EQUALS"; colinc(columnNum, yytext); T.EQUALS(!lineNum, !columnNum - 6, yypos, yypos + String.size yytext));
"IMPLIES"		 => (append token_list "IMPLIES" "IMPLIES"; colinc(columnNum, yytext); T.IMPLIES(!lineNum, !columnNum - 7, yypos, yypos + String.size yytext));
"IF"			 => (append token_list "IF" "IF"; colinc(columnNum, yytext); T.IF(!lineNum, !columnNum - 2, yypos, yypos + String.size yytext));
"THEN"           => (append token_list "THEN" "THEN"; colinc(columnNum, yytext); T.THEN(!lineNum, !columnNum - 4, yypos, yypos + String.size yytext));
"ELSE"			 => (append token_list "ELSE" "ELSE"; colinc(columnNum, yytext); T.ELSE(!lineNum, !columnNum - 4, yypos, yypos + String.size yytext));
"TRUE" | "FALSE" => (append token_list "CONST" yytext; colinc(columnNum, yytext); T.CONST(yytext, !lineNum, !columnNum - String.size yytext, yypos, yypos + String.size yytext));
{alpha}+         => (append token_list "ID" yytext; colinc(columnNum, yytext); T.ID(yytext, !lineNum, !columnNum - String.size yytext, yypos, yypos + String.size yytext));
.                => (error(!lineNum, !columnNum, yytext); colinc(columnNum, yytext); lex());
