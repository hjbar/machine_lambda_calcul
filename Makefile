default: clear clean fmt build exec

clean:
	@dune clean

full_clean:
	@dune clean
	@rm -rf tests/*

build:
	@dune build

fmt:
	@dune fmt

clear:
	@clear

exec:
	@dune exec bin/main.exe
