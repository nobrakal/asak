let rec read_all_pkg desc =
  try
    let str = Unix.readdir desc in
    if str = ".." || str = "."
    then read_all_pkg desc
    else str::read_all_pkg desc
  with
  | End_of_file -> []

let test_problem should_fail x res =
  if not (res = 0)
  then
    let str = "PROBLEM WITH: " ^ x in
    if should_fail
    then failwith str
    else print_endline str; true
  else false

let test_comand x =
  let res = Sys.command (x ^ " --show-actions | grep \"remove[ ]*ocaml-variants\"")
  in not (res = 0)

let install_pkg prefix x =
  let cmd = "opam install -y --switch=" ^ prefix ^ " " ^ x in
  if not (test_comand cmd)
  then
    begin print_endline ("PROBLEM WITH: " ^ x); false end
  else
    let res = Sys.command cmd in
    not (test_problem false x res)

let mk_pred cmp x y =
  match x with
  | None -> true
  | Some x -> cmp x y

let main prefix dir from top =
  let desc = Unix.opendir dir in
  let from = mk_pred (fun x y -> x <= y) from in
  let top  = mk_pred (fun x y -> x >= y) top in
  let pkg_list =
    List.sort compare @@
      List.filter (fun x -> from x && top x) @@
        read_all_pkg desc in
  Unix.closedir desc;
  let lst_ok = List.filter (install_pkg prefix) pkg_list in
  print_endline "INSTALLED PKGS";
  List.iter print_endline lst_ok

let () = main Sys.argv.(1) Sys.argv.(2)
           (try Some (Sys.argv.(3)) with | Invalid_argument _ -> None)
           (try Some (Sys.argv.(4)) with | Invalid_argument _ -> None)
