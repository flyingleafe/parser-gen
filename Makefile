.PHONY: all clean

all: variables

variables: VariablesParser.hs Variables.hs
	ghc Variables.hs -o variables

VariablesParser.hs: Variables.fgen
	stack exec parser-gen-exe Variables.fgen > VariablesParser.hs

clean:
	rm -f variables VariablesParser.hs *.o *.hi


