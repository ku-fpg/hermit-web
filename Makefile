boot:
	cabal install --force-reinstalls

ghci:
	ghc --interactive -isrc/ -package ghc -Wall -fno-warn-orphans src/HERMIT/Web.hs

test:
	hermit-web ../hermit/examples/last/Last.hs &
	sleep 10
	curl -X POST http://localhost:3000/connect # user 1
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":0,"ast":0},"cmd":"down"}'
	curl -X POST http://localhost:3000/connect # user 2
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":1,"ast":1},"cmd":"down"}'
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":1,"ast":2},"cmd":"set-pp-type Show"}'
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":0,"ast":1},"cmd":"down"}'
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":1,"ast":2},"cmd":"down"}'
	curl -X POST http://localhost:3000/command -H "content-type: application/json" -d '{"token":{"user":0,"ast":3},"cmd":"resume"}'
