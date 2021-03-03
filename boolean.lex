structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token


val lineNum = ref 1
val columnNum = ref 1
val token_list = ref []
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
val error = 
	fn x y str => 
		TextIO.output(TextIO.stdOut, "Unknown Token:" ^ Int.toString(!x) ^ ":" ^ Int.toString(!y) ^ ":" ^ str ^ "\n")
val eof = fn () => (print "["; printString(reverse(!token_list)); print "]\n"; Tokens.EOF(yypos, yypos + String.size yytext))	
val colinc = fn x str => (x := !x + String.size str)			   

%%

%header (functor booleanLexFun(structure Tokens: boolean_TOKENS));
alpha = [A-Za-z];
ws = [\ \t];

%%
\n               => (lineNum := !lineNum + 1; columnNum := 1; lex());
{ws}+            => (colinc columnNum yytext; lex());
";"      		 => (append token_list "TERM" ";"; colinc columnNum yytext; T.TERM(yypos, yypos + 1));
"("				 => (append token_list "LPAREN" "("; colinc columnNum yytext; T.LPAREN(yypos, yypos + 1));
")"				 => (append token_list "RPAREN" ")"; colinc columnNum yytext; T.RPAREN(yypos, yypos + 1));
"NOT"			 => (append token_list "NOT" "NOT"; colinc columnNum yytext; T.NOT(yypos, yypos + 3));
"AND"            => (append token_list "AND" "AND"; colinc columnNum yytext; T.AND(yypos, yypos + 3));
"OR"             => (append token_list "OR" "OR"; colinc columnNum yytext; T.OR(yypos, yypos + 2));
"XOR"			 => (append token_list "XOR" "XOR"; colinc columnNum yytext; T.XOR(yypos, yypos + 3));
"EQUALS"		 => (append token_list "EQUALS" "EQUALS"; colinc columnNum yytext; T.EQUALS(yypos, yypos + 6));
"IMPLIES"		 => (append token_list "IMPLIES" "IMPLIES"; colinc columnNum yytext; T.IMPLIES(yypos, yypos + 7));
"IF"			 => (append token_list "IF" "IF"; colinc columnNum yytext; T.IF(yypos, yypos + 2));
"THEN"           => (append token_list "THEN" "THEN"; colinc columnNum yytext; T.THEN(yypos, yypos + 4));
"ELSE"			 => (append token_list "ELSE" "ELSE"; colinc columnNum yytext; T.ELSE(yypos, yypos + 4));
"TRUE" | "FALSE" => (append token_list "CONST" yytext; colinc columnNum yytext; T.CONST(yypos, yypos + String.size yytext);
{alpha}+         => (colinc columnNum yytext; T.ID(yytext, yypos, yypos + String.size yytext);
.                => (error lineNum columnNum yytext; colinc columnNum yytext; lex());
