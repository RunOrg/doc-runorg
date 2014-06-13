(* © 2014 RunOrg *)

module String = BatString
let (//) = Filename.concat

(** Traversing all directories at the target location, extracting a list of 
    files and their contents. *)

(** The source path. Expected as the first command-line argument. *)
let source_path = 
  if Array.length Sys.argv < 3 then ( 
    print_endline "Usage: doc-runorg [inpath] [outpath]" ;
    exit (-1)) ;
  Sys.argv.(1)

(** Create a list of all *.md files inside the source directory. Recurses
    into sub-directories. All paths are relative to the source path. *)
let all_md = 
  let rec recurse path acc file = 
    let file_path = path // file in 
    if String.ends_with file ".md" then file_path :: acc else
      let full_path = source_path // file_path in
      if Sys.is_directory full_path then 
	Array.fold_left (recurse file_path) acc (Sys.readdir full_path) 
      else
	acc
  in

  recurse "" [] ""

(** Load [Read.t] for every file. *)
let all_data = 
  List.map (fun path -> Read.parse (source_path // path) path) all_md
