default: clean fmt build clear exec

clean:
	dune clean

build:
	dune build

fmt:
	dune fmt

clear:
	clear

exec:
	dune exec bin/main.exe
