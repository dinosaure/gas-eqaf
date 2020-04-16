let fuzz_camlEqaf__equal_155 prgm =
  Crowbar.add_test ~name:"camlEqaf__equal_155" Crowbar.[ bytes; bytes; ] @@ fun v0 v1 ->
  let expect_res = String.equal v0 v1 in
  match Run.camlEqaf__equal_155 prgm v0 v1 with
  | Ok (res, _ticks) ->
    Crowbar.check_eq res expect_res
  | Error (`Msg err) -> failwith err

let fuzz_camlEqaf__equal_155 prgm =
  try fuzz_camlEqaf__equal_155 prgm ; Ok ()
  with Failure err -> Error (`Msg err)

