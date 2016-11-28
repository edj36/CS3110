test:
	ocamlbuild -pkgs oUnit,str,unix,yojson scrabble_test.byte && ./scrabble_test.byte

play: 
	ocamlbuild -pkgs str,unix game.byte && ./game.byte

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
