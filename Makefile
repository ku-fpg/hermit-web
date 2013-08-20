boot:
	cabal install --force-reinstalls

ghci:
	ghc --interactive -isrc/ -package ghc -Wall -fno-warn-orphans src/HERMIT/Web.hs
