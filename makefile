
bin/circletree: *.hs
	cabal build
	rm -rf bin
	mkdir bin
	mkdir -p eg
	mv dist/build/circletree/circletree bin/circletree

run: bin/circletree
	bin/circletree
