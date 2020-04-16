open Rresult

let ( <.> ) f g = fun x -> f (g x)

module Interpreter = Parser.MenhirInterpreter

let pp_position ppf { Lexing.pos_lnum; Lexing.pos_cnum; Lexing.pos_bol; _ } =
  Format.fprintf ppf "l.%d c.%d" pos_lnum (pos_cnum - pos_bol)

let stand_alone_eqaf =
  let open Bos in
  let cmd = Cmd.(v "ocamlfind" %  "query" % "eqaf") in
  let out = OS.Cmd.run_out cmd in
  match OS.Cmd.out_string ~trim:true out with
  | Ok (path, (_, `Exited 0)) -> Fpath.v path
  | Ok (err, _) -> Fmt.failwith "ocamlfind: %s" err
  | Error (`Msg err) -> Fmt.failwith "ocamlfind: %s" err

let compile_interface mli =
  let open Bos in
  let cmi = Fpath.(set_ext "cmi" mli) in
  let cmd = Cmd.(v "ocamlopt" % "-c" % Fpath.(to_string mli) % "-o" % (Fpath.to_string cmi)) in
  OS.Cmd.run cmd >>| fun () -> OS.Path.must_exist cmi >>| fun cmi ->
  Fmt.pr "[%a] %a compiled.\n%!" Fmt.(styled `Green string) "x" Fpath.pp cmi ; cmi

let generate_assembly_file ~mli ml =
  let open Bos in
  let cmx = Fpath.(set_ext "cmx" ml) in
  let cmd = Cmd.(v "ocamlopt"
                 % "-intf" % Fpath.(to_string mli)
                 % "-S"
                 % "-I" % Fpath.(to_string (parent mli))
                 % "-c" % Fpath.(to_string ml)
                 % "-o" % Fpath.(to_string cmx)) in
  OS.Cmd.run cmd >>= fun () -> OS.Path.must_exist (Fpath.set_ext "s" ml) >>| fun s ->
  Fmt.pr "[%a] %a compiled.\n%!" Fmt.(styled `Green string) "x" Fpath.pp cmx ;
  Fmt.pr "[%a] %a generated.\n%!" Fmt.(styled `Green string) "x" Fpath.pp s ; s

let parse_assembly filename =
  Bos.OS.File.with_ic filename @@ fun ic () ->
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  let lexer () =
    let token = Lexer.parser lexbuf in
    (token, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
  let rec go (checkpoint : Prgm.unresolved list Interpreter.checkpoint) =
    match checkpoint with
    | Interpreter.InputNeeded _env ->
      let token, start, stop = lexer () in
      let checkpoint = Interpreter.offer checkpoint (token, start, stop) in
      go checkpoint
    | Interpreter.Shifting _ | Interpreter.AboutToReduce _ ->
      let checkpoint = Interpreter.resume checkpoint in
      go checkpoint
    | Interpreter.HandlingError env ->
      let start, stop = Interpreter.positions env in
      R.reword_error (R.msgf "Error on %a at %a" Fpath.pp filename
                        Fmt.(pair pp_position pp_position)) (Error (start, stop))
    | Interpreter.Accepted t -> Ok t
    | Interpreter.Rejected -> assert false in
  go ((Parser.Incremental.main <.> Lexing.lexeme_start_p) lexbuf)

let eqaf_prgm =
  let open Bos in
  let open Rresult in
  OS.Dir.with_tmp "eqaf-%s" @@ fun sandbox () ->
  OS.Path.symlink ~target:Fpath.(stand_alone_eqaf /  "eqaf.ml") Fpath.(sandbox /  "eqaf.ml") >>= fun () ->
  OS.Path.symlink ~target:Fpath.(stand_alone_eqaf / "eqaf.mli") Fpath.(sandbox / "eqaf.mli") >>= fun () ->
  compile_interface Fpath.(sandbox / "eqaf.mli") >>= fun _cmi ->
  generate_assembly_file ~mli:Fpath.(sandbox / "eqaf.mli") Fpath.(sandbox / "eqaf.ml") >>= fun s ->
  parse_assembly s () |> R.join

let output_assembly filename prgm =
  Bos.OS.File.with_oc filename @@ fun oc () ->
  let ppf = Format.formatter_of_out_channel oc in
  List.iter (Prgm.pp_unresolved ppf) prgm ; Ok ()

let prelude output =
  eqaf_prgm () |> R.join >>= fun prgm ->
  output_assembly output prgm () |> R.join >>= fun () ->
  Fmt.pr "[%a] %a generated.\n%!" Fmt.(styled `Green string) "x" Fpath.pp output ;
  Ok prgm

let () =
  let fiber =
    prelude (Fpath.v "eqaf.s") >>= Fuzz.fuzz_camlEqaf__equal_155 in
  R.failwith_error_msg fiber
