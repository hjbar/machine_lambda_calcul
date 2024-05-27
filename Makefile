default: clean build clear exec

clean:
	dune clean

build:
	dune build

clear:
	clear

exec:
	dune exec bin/main.exe
