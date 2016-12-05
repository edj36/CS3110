test:
	ocamlbuild -pkgs oUnit,str,unix,yojson,ANSITerminal,radixtree scrabble_test.byte && ./scrabble_test.byte

play: 
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree,cohttp.lwt,atdgen game.byte && ./game.byte

client:
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree,cohttp.lwt,atdgen scrabble_client.native && ./scrabble_client.native

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
