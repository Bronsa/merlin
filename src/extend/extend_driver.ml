module P = Extend_protocol

(** Helper for the driver (Merlin) *)

type t = {
  name: string;
  capabilities: P.capabilities;
  stdin: out_channel;
  stdout: in_channel;

  notify: string -> unit;
  debug: string -> unit;
}

exception Extension of string * string * string

let run ?(notify=ignore) ?(debug=ignore) name =
  let (stdout, stdin) = Unix.open_process ("ocamlmerlin-" ^ name) in
  match Extend_main.Handshake.negotiate_driver name stdout stdin with
  | capabilities -> {name; capabilities; stdin; stdout; notify; debug}
  | exception exn ->
    close_out_noerr stdin;
    close_in_noerr stdout;
    raise exn

let stop t =
  close_out_noerr t.stdin;
  close_in_noerr t.stdout

let capabilities t = t.capabilities

let reader t request =
  output_value t.stdin (P.Reader_request request);
  flush t.stdin;
  let rec aux () =
    match input_value t.stdout with
    | P.Notify str -> t.notify str; aux ()
    | P.Debug str -> t.debug str; aux ()
    | P.Exception (kind, msg) ->
      stop t;
      raise (Extension (t.name, kind, msg))
    | P.Reader_response response ->
      response
  in
  aux ()
