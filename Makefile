.PHONY: all clean

all: variables expressions

variables: VariablesParser.hs Variables.hs
	ghc Variables.hs -o $@

expressions: ExpressionsParser.hs Expressions.hs
	ghc Expressions.hs -o $@

%Parser.hs: Variables.fgen
	stack exec parser-gen-exe $*.fgen > $@

clean:
	rm -f expressions variables *Parser.hs *.o *.hi


