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
dist/build/testGrammar/testGrammar: abs-frontend/src/ABS.cf 
# run when grammar changes
	@mkdir -p dist/build/buildGrammar/
	cp abs-frontend/src/ABS.cf dist/build/buildGrammar/
# generate haskell-source parser, lexer, and helper code
	cd dist/build/buildGrammar; bnfc -haskell -p "Lang.ABS.Compiler.BNFC" ABS.cf 
# move the generated grammar documentation
	mv dist/build/buildGrammar/Lang/ABS/Compiler/BNFC/DocABS.txt doc 
# generate haskell parser
	cd dist/build/buildGrammar; happy -gca Lang/ABS/Compiler/BNFC/ParABS.y; 
# generate haskell lexer
	cd dist/build/buildGrammar; alex -g Lang/ABS/Compiler/BNFC/LexABS.x 
	@mkdir -p dist/build/testGrammar/
# compiles the Haskell parse-ABS-example
	cd dist/build/buildGrammar;ghc --make Lang/ABS/Compiler/BNFC/TestABS.hs -o ../testGrammar/testGrammar; 
	cd dist/build/buildGrammar/Lang/ABS/Compiler/BNFC/; rm -rf *.o *.hi *.x *.y *.bak TestABS.hs  # cleanup
# move the generated haskell files to this src/
	cp -r dist/build/buildGrammar/Lang/ABS/Compiler/BNFC src/Lang/ABS/Compiler 

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
