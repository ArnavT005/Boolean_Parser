ML = mlton

LEX = mllex

YCC = mlyacc

NAME = a2

BOOL = boolean

FILES = Error lastToken Yes

.PHONY: all
all: $(NAME)


$(NAME): $(NAME).mlb
	$(ML) $(NAME).mlb

$(NAME).mlb: $(BOOL).yacc.sig $(BOOL).yacc.sml $(BOOL).lex.sml

$(BOOL).yacc.sig: $(BOOL).yacc
	$(YCC) $(BOOL).yacc

$(BOOL).yacc.sml: $(BOOL).yacc		
	$(YCC) $(BOOL).yacc

$(BOOL).lex.sml: $(BOOL).lex
	$(LEX) $(BOOL).lex

.PHONY: clean
clean:
	rm $(BOOL).yacc.* $(BOOL).lex.* $(NAME) $(FILES) 