test:
	ocamlbuild -pkgs oUnit,str,unix,yojson,ANSITerminal,radixtree scrabble_test.byte && ./scrabble_test.byte

play: 
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree game.byte && ./game.byte

client:
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree,cohttp.lwt scrabble_client.native && ./scrabble_client.native

server: 
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree,cohttp.lwt scrabble_server.native && ./scrabble_server.native

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
