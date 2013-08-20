boot:
	cabal install --force-reinstalls

ghci:
	ghc --interactive -isrc/ -package ghc src/HERMIT/Web.hs
