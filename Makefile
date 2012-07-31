.PHONY: all

all: termplot

termplot: src/Main.hs
	ghc -Wall -threaded -o termplot --make src/Main.hs
