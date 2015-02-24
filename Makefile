PREFIX=/usr/local/bin

default:
	@echo "Type: 'sudo make install' to make the installation"

install: deps
	cd haxr-browser; cabal install --force-reinstalls
	cd opennebula; cabal install --force-reinstalls
	cabal install --force-reinstalls --bindir=${PREFIX}

deps: 
	-git submodule init
	git submodule update # clone git submodules
	cabal update
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
	-rm -rf dist/
	-rm -f src/ParABS.y src/LexABS.x src/TestABS.* src/*.bak # cleanup bnfc intermediate code
	-rm -f src/*.hi src/*.o examples/*.hi examples/*.o # remove any compiled example
	-rm -f doc/*.html

doc: 
	pandoc -t html -s README.md -o README.html	
	pandoc -t html -s doc/TODO.md -o doc/TODO.html
	pandoc -f t2t -t html -s doc/DocABS.txt -o doc/GrammarDocumentation.html

.PHONY: default install deps test grammar clean doc
