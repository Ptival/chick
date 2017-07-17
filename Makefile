.PHONY: all clean

COMPILER=ghc
#COMPILER=ghcjs

# TODO: Make this better once it builds
all: chick.nix # Main.jsexe

Main.jsexe: Main.hs # Lexer.hs Parser.hs
	$(COMPILER) Main.hs

Lexer.hs: Lexer.x
	alex $<

Parser.hs: Parser.y
	happy $<

chick.nix: chick.cabal
	cabal2nix . > $@

clean:
	rm -f Lexer.hs Parser.hs
	rm -f *.hi *.o *.js_hi *.js_o
	rm -f chick.nix
