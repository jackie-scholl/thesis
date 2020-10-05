all:
	#!/usr/bin/env fish
	cd ./tic_tac_toe
	rm -f Haskell.hi
	ghc -Wall -fno-warn-tabs -fno-warn-missing-signatures \
		-fdiagnostics-color=always -o out/haskell_exec  Haskell.hs 2>&1 \
		| tee out/haskell_compile.txt
	./out/haskell_exec &>out/haskell_run.txt
	racket -t racket.rkt &>out/racket_run.txt

