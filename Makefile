build:
	dune build

clean:
	dune clean

play:
	dune exec bin/main.exe

test:
	dune exec test/test.exe

	