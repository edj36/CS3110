open Lwt
open Cohttp
open Cohttp_lwt_unix
open Data_j

(* https://github.com/mirage/ocaml-cohttp/tree/master/examples *)

let setup s = 
  let u = Uri.of_string "http://127.0.0.1:8080/" in 
  Client.get u >>= fun (resp, body) -> 
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code; 
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let update m s = 
  let m_str = string_of_move m in 
  let s_str = string_of_game_state s in 
  post_move (m_str ^ "|" ^ s_str)


let get_state = 
	let u = Uri.of_string "http://127.0.0.1:8080/" in 
  Client.get u >>= fun (resp, body) -> 
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code; 
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let post_move str = 
  let b = Cohttp_lwt_body.of_string str in 
  let u = Uri.of_string "http://127.0.0.1:8080/" in 
  Client.post ~body: b u >>= fun (resp, body) -> 
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code; 
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  game_state_of_string body 

(*let () =
  let body = Lwt_main.run (post_move "hello world") in
  let body = Lwt_main.run (get_state) in 
  print_endline ("Received body:\n" ^ body)*)