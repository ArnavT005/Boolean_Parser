ML = mlton

LEX = ml-lex

YCC = ml-yacc

NAME = a2

BOOL = boolean

COMP = compile

PARSE = parse.sml

.PHONY: all
all: $(NAME)


$(NAME): $(COMP).mlb
	$(ML) $(COMP).mlb
	mv $(COMP) $(NAME)

$(COMP).mlb: $(BOOL).yacc.sig $(BOOL).yacc.sml $(BOOL).lex.sml $(PARSE)

$(BOOL).yacc.sig: $(BOOL).yacc
	$(YCC) $(BOOL).yacc

$(BOOL).yacc.sml: $(BOOL).yacc		
	$(YCC) $(BOOL).yacc

$(BOOL).lex.sml: $(BOOL).lex
	$(LEX) $(BOOL).lex

.PHONY: clean
clean:
	rm $(BOOL).yacc.* $(BOOL).lex.* $(NAME)