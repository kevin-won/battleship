open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    match req |> Request.meth with
    | `GET ->
        ( body |> Cohttp_lwt.Body.to_string >|= fun body ->
          print_endline "GET REQUEST RECEIVED";
          body )
        >>= fun body -> Server.respond_string ~status:`OK ~body ()
    | `POST ->
        ( body |> Cohttp_lwt.Body.to_string >|= fun body ->
          print_endline "POST REQUEST RECEIVED";
          body )
        >>= fun body -> Server.respond_string ~status:`OK ~body ()
    | _ ->
        ( body |> Cohttp_lwt.Body.to_string >|= fun body ->
          print_endline "UNEXPECTED REQUEST RECEIVED";
          "ERROR" )
        >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port 8080)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
