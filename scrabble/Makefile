test:
	ocamlbuild -pkgs oUnit,str,unix,yojson,ANSITerminal,radixtree scrabble_test.byte && ./scrabble_test.byte

play: 
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree game.byte && ./game.byte

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
