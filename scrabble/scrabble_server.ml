open Lwt
open Cohttp
open Cohttp_lwt_unix

(* https://github.com/mirage/ocaml-cohttp/tree/master/examples *)

let state_to_string s = s ^ "stringify"

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let headers = req |> Request.headers |> Header.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    match req |> Request.meth with 
    | `GET -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        ("GET REQUEST RECEIVED: \n" ^ body ^ "\n"))
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))
    | `POST -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        ("POST REQUEST RECEIVED: \n" ^ body ^ "\n")) 
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))
    | _ -> body |> Cohttp_lwt_body.to_string >|= (fun body ->
        ("ERROR\n"))
      >>= (fun body -> (Server.respond_string ~status:`OK ~body ()))  
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
