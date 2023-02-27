.PHONY: all automata distclean

all: automata

automata:
	dune build ./src/automata.exe

distclean:
	dune clean
