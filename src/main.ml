open Core

(* configuration *)

let fromager_config = "fromage.toml"

type config = {
  ocamlformat_version : string option;
  ocamlformat_options : (string * string) list;
  ignored_files : string list;
  ignored_dirs : string list;
}

let parse_toml filename : config option =
  let toml_value_to_string v =
    (* Avoid quoting strings since ocamlformat normally uses then un-quotes *)
    match v with Otoml.TomlString s -> s | _ -> Otoml.Printer.to_string v
  in
  match Sys.file_exists filename with
  | `No | `Unknown -> None
  | `Yes ->
      let toml =
        match Otoml.Parser.from_file_result filename with
        | Error msg ->
            Printf.ksprintf failwith "could not parse fromage.toml:\n%s" msg
        | Ok toml -> toml
      in
      let config =
        match Otoml.find_opt toml Otoml.get_table [ "config" ] with
        | Some config -> Otoml.table config
        | _ -> failwith "fromage.toml has no config table"
      in
      let ocamlformat_version =
        match
          Otoml.find_opt config Otoml.get_string [ "ocamlformat_version" ]
        with
        | Some _ as ov -> ov
        | _ -> None
      in
      let ignored_files =
        match
          Otoml.find_opt config
            (Otoml.get_array Otoml.get_string)
            [ "ignored_files" ]
        with
        | Some ignored_files -> ignored_files
        | _ -> []
      in
      let ignored_dirs =
        match
          Otoml.find_opt config
            (Otoml.get_array Otoml.get_string)
            [ "ignored_dirs" ]
        with
        | Some ignored_dirs -> ignored_dirs
        | _ -> []
      in
      let ocamlformat_options =
        let translate_key k =
          String.map k ~f:(fun c -> match c with '_' -> '-' | _ -> c)
        in
        let opts_table =
          Otoml.find_opt toml Otoml.get_table [ "ocamlformat_options" ]
        in
        match opts_table with
        | None -> []
        | Some kvs ->
            List.map kvs ~f:(fun (k, v) ->
                (translate_key k, toml_value_to_string v))
      in
      Some
        {
          ocamlformat_version;
          ocamlformat_options;
          ignored_files;
          ignored_dirs;
        }

(* interaction with ocamlformat *)

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  let res = Unix.close_process_in inp in
  match res with
  | Ok _ -> r
  | Error (`Exit_non_zero 127) ->
      Printf.ksprintf failwith "failed to run %s: command not found" cmd
  | Error _ ->
      Printf.ksprintf failwith "failed to run %s: command exited with an error"
        cmd

let ocamlformat args =
  let args = String.concat ~sep:" " args in
  let command = "ocamlformat " ^ args in
  run command

let ocamlformat_version = ocamlformat [ "--version" ]

let run_ocamlformat ~(write : bool) (filename : string) =
  let args = if write then [ "-i" ] else [ "" ] in
  (* write in-place *)
  let args = args @ [ filename ] in
  let _ = ocamlformat args in
  ()

(* recursion *)

let is_ignored_dir (path : string) ~config =
  let basename = Filename.basename path in
  let first_letter = basename.[0] in
  (* ignore _* and .* folders *)
  if
    Char.(first_letter = '_')
    || (String.length basename > 1 && Char.(first_letter = '.'))
  then true
  else
    match config with
    | None -> false
    | Some config -> List.mem ~equal:String.( = ) config.ignored_dirs path

let is_ignored_file path ~config =
  let basename = Filename.basename path in
  let first_letter = basename.[0] in
  (* ignore .* files *)
  if Char.(first_letter = '.') then true
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

let enforce_ocamlformat_version config =
  match config with
  | None -> ()
  | Some config -> (
      match config.ocamlformat_version with
      | None -> ()
      | Some expected_version ->
          let version = String.rstrip ocamlformat_version in
          if String.(version = expected_version) then ()
          else failwith "unexpected ocamlformat version according to config")

let file_exists file =
  match Sys.file_exists file with `Yes -> true | _ -> false

let create_ocamlformat_config config file =
  let ocamlformat_config =
    match config with
    | Some config ->
        let opts =
          List.map config.ocamlformat_options ~f:(fun (k, v) ->
              Printf.sprintf "%s=%s" k v)
        in
        String.concat ~sep:"\n" opts
    | None -> ""
  in
  Out_channel.write_all file ~data:ocamlformat_config

let () =
  let cwd = Sys.getcwd () in
  (* parse fromage.toml *)
  let config = parse_toml fromager_config in
  (* enforce ocamlformat version *)
  enforce_ocamlformat_version config;
  (* create an empty .ocamlformat if there are none *)
  let ocamlformat_file = Filename.concat cwd ".ocamlformat" in
  let ocamlformat_already_exists = file_exists ocamlformat_file in
  if not ocamlformat_already_exists then
    create_ocamlformat_config config ocamlformat_file;
  (* format everything *)
  visit [ "." ] ~config ~f:(run_ocamlformat ~write:true);
  (* remove the .ocamlformat if there were none *)
  if not ocamlformat_already_exists then Unix.remove ocamlformat_file
