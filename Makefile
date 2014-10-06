PREFIX=/usr/local/bin

build: deps
	cabal install --only-dependencies --force-reinstalls
	cabal configure
	cabal build

install:
	cp dist/build/abs2haskell/abs2haskell ${PREFIX}

deps: 
	-git submodule init
	git submodule update # clone git submodules
	-cabal sandbox init
	cabal update
	cabal sandbox add-source haxr-browser
	cabal sandbox add-source opennebula
	cabal install happy-1.19.4

test:
	cabal install --only-dependencies --enable-tests
	cabal test

grammar: dist/build/testGrammar/testGrammar
dist/build/testGrammar/testGrammar: src/ABS.cf # run when grammar changes
	cd src; bnfc -haskell ABS.cf # generates haskell-source parser, lexer, and helper code
	cd src; mv DocABS.txt ../doc # move the generated grammar documentation
	cd src; happy -gca ParABS.y; alex -g LexABS.x # generates Haskell parse-example to parse ABS code
	@mkdir -p dist/build/testGrammar/
	cd src; ghc --make TestABS.hs -o ../dist/build/testGrammar/testGrammar; rm -rf *.o *.hi TestABS.hs # compiles the Haskell parse-ABS-example

clean:
	-cabal sandbox delete
	-rm -rf dist/
	-rm -f src/ParABS.y src/LexABS.x src/TestABS.* src/*.bak # cleanup bnfc intermediate code
	-rm -f src/*.hi src/*.o examples/*.hi examples/*.o # remove any compiled example
	-rm -f doc/*.html

doc: 
	pandoc -t html -s README.md -o README.html	
	pandoc -t html -s doc/TODO.md -o doc/TODO.html
	pandoc -f t2t -t html -s doc/DocABS.txt -o doc/GrammarDocumentation.html

.PHONY: build install deps test grammar clean doc
