.PHONY: all

all: termplot

termplot: Main.hs
	ghc -Wall -o termplot --make Main
