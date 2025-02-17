open Lwt_unix
open Lwt.Infix

let buffer_size = 1024
let gemini_port = 1965

let empty_authenticator : X509.Authenticator.t =
  (* TODO: implement certification checks *)
  fun ?ip:_ ~host:_ _certs -> (Ok (None))

let rec read_loop tls_client =
  let buffer = Bytes.create buffer_size in
  let%lwt len = Tls_lwt.Unix.read tls_client buffer in
  if len = 0 then 
    Lwt.return_unit  (* Stop reading when no more data *)
  else
    let response = Bytes.sub_string buffer 0 len in
    let%lwt () = Lwt_io.printl response in
    read_loop tls_client  (* Continue reading *)

let gemini_request host path =
  let request = Printf.sprintf "gemini://%s%s\r\n" host path in
  (* Resolve hostname *)
  let%lwt addresses = Lwt_unix.getaddrinfo host (string_of_int gemini_port) [AI_FAMILY PF_INET] in
  let sockaddr = (List.hd addresses).ai_addr in

  (* Create Lwt-aware socket *)
  let fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in

  (* TLS setup *)
  (* let%lwt authenticator = X509_lwt.authenticator (`Ca_dir "/etc/ssl/certs") in *)
  let config = match Tls.Config.client ~authenticator:empty_authenticator () with
    | Ok c -> c
    | Error (`Msg e) -> failwith ("Error while creating TLS config: " ^ e)
  in

  let%lwt () = Lwt_unix.connect fd sockaddr in

  let%lwt tls_client = Tls_lwt.Unix.client_of_fd config fd in

  (* Send Gemini request *)
  let%lwt () = Tls_lwt.Unix.write tls_client request in

  (* Receive response *)
  (* Read response in a loop *)
  read_loop tls_client >>= fun () ->

  (* Close connection *)
  Tls_lwt.Unix.close tls_client

(* Run the request *)
let () =
  Lwt_main.run (gemini_request "astrobotany.mozz.us" "/")

