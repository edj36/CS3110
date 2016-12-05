test:
	ocamlbuild -pkgs oUnit,str,unix,yojson,ANSITerminal,radixtree scrabble_test.byte && ./scrabble_test.byte

server: 
	ocamlbuild -pkgs str,unix,yojson,ANSITerminal,radixtree,cohttp.lwt,atdgen scrabble_server.native && ./scrabble_server.native

check:
	bash checkenv.sh 

clean:
	ocamlbuild -clean
