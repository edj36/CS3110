open Lwt
open Cohttp
open Cohttp_lwt_unix
open Utils
open Data_t
open Data_j
open State

(* https://github.com/mirage/ocaml-cohttp/tree/master/examples *)

let server =
  let empty_state = 
    {
      board = initilize_board ();
      score_board = [];
      letter_bag = [];
      player_racks = [];
      turn = 0;
      counter = 0;
      quit = false; 
    } in 
  let current_state = ref empty_state in 
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    match req |> Request.meth with 
    | `GET -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        (*("GET REQUEST RECEIVED: \n" ^ (string_of_int (!count)) ^ "\n")) *)
        (Data_j.string_of_game_state (!current_state)))
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))
    | `POST -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        (*count := (!count) + 1; 
        ("POST REQUEST RECEIVED: \n" ^ (string_of_int (!count)) ^ "\n")) *)
        if (body.[0] = '{') then
          let r = Str.regexp "\|" in 
          let o = (Str.split r body) in 
          let mov = Data_j.move_of_string (get_nth (o,0)) in 
          let st = Data_j.game_state_of_string (get_nth (o,1)) in 
          let new_st = update mov st in 
          current_state := new_st;
          Data_j.string_of_game_state new_st
        else 
          let new_st = setup body in 
          if (new_st = "error") then 
            new_st
          else 
            let _ = current_state := (Data_j.game_state_of_string new_st) in 
            new_st
         )
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))
    | _ -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        ("ERROR"))
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))  
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
