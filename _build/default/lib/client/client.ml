open Lwt
open Cohttp
open Cohttp_lwt_unix

let get_request =
  let u = Uri.of_string "host-name" in
  Client.get u >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let post_request str =
  let b = Cohttp_lwt_body.of_string str in
  let u = Uri.of_string "host-name" in
  Client.post ~body:b u >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body
