open Core

(* configuration *)

let fromager_config = "fromage.toml"

type config = { ignored_files : string list; ignored_dirs : string list }

let parse_toml filename : config option =
  match Sys.file_exists filename with
  | `No | `Unknown -> None
  | `Yes ->
      let toml =
        match Toml.Parser.from_filename filename with
        | `Error _ -> failwith "could not parse fromage.toml"
        | `Ok toml -> toml
      in
      let config =
        match Toml.Types.Table.find_opt (Toml.Min.key "config") toml with
        | Some (TTable config) -> config
        | _ -> failwith "fromage.toml has no config table"
      in
      let ignored_files =
        match
          Toml.Types.Table.find_opt (Toml.Min.key "ignored_files") config
        with
        | Some (TArray (NodeString ignored_files)) -> ignored_files
        | _ -> []
      in
      let ignored_dirs =
        match
          Toml.Types.Table.find_opt (Toml.Min.key "ignored_dirs") config
        with
        | Some (TArray (NodeString ignored_dirs)) -> ignored_dirs
        | _ -> []
      in
      Some { ignored_files; ignored_dirs }

(* interaction with ocamlformat *)

let ocamlformat args =
  let args = String.concat ~sep:" " args in
  let command = "ocamlformat " ^ args in
  Sys.command_exn command

let run_ocamlformat ~(write : bool) (filename : string) =
  let args = if write then [ "-i" ] else [ "" ] in
  (* write in-place *)
  let args = args @ [ filename ] in
  let _ = ocamlformat args in
  ()

(* recursion *)

let is_ignored_dir (path : string) ~config =
  let path = Filename.basename path in
  let first_letter = path.[0] in
  (* ignore _* and .* folders *)
  if
    Char.(first_letter = '_')
    || (String.length path > 1 && Char.(first_letter = '.'))
  then true
  else
    match config with
    | None -> false
    | Some config -> List.mem ~equal:String.( = ) config.ignored_dirs path

let is_ignored_file path ~config =
  let path = Filename.basename path in
  (* ignore .* files *)
  if Char.(path.[0] = '.') then true
  else
    match config with
    | None -> false
    | Some config -> List.mem ~equal:String.( = ) config.ignored_files path

(* apply [~f] on every `.ml` and `.mli` file found in [folders] *)
let rec visit folders ~config ~f =
  match folders with
  | [] -> ()
  | file :: rest -> (
      let is_dir = Sys.is_directory file in
      match is_dir with
      | `Yes ->
          let rest =
            let ignored = is_ignored_dir ~config file in
            if not ignored then
              let inside_dirs = Sys.ls_dir file in
              let inside_dirs =
                List.map inside_dirs ~f:(fun x -> Filename.concat file x)
              in
              inside_dirs @ rest
            else rest
          in
          visit rest ~config ~f
      | `No | `Unknown ->
          let ignored = is_ignored_file ~config file in
          (if not ignored then
           match Filename.split_extension file with
           | _, Some "ml" | _, Some "mli" -> f file
           | _ -> ());
          visit rest ~config ~f)

(* main *)

let () =
  let cwd = Sys.getcwd () in

  (* parse fromage.toml *)
  let config = parse_toml fromager_config in

  (* create an empty .ocamlformat *)
  let ocamlformat_file = Filename.concat cwd ".ocamlformat" in
  Out_channel.write_all ocamlformat_file ~data:"";

  (* format everything *)
  visit [ "." ] ~config ~f:(run_ocamlformat ~write:true);

  (* remove the .ocamlformat *)
  Unix.remove ocamlformat_file
