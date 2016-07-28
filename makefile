
build:
	cabal build
	rm -rf bin
	mkdir bin
	mv dist/build/circletree/circletree bin/circletree

run:
	bin/circletree