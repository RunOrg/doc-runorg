(* © 2014 RunOrg *)

module String = BatString
let (//) = Filename.concat

(** Generates and writes individual files to the disk. *)

(** The target path. Expected as the second command-line argument. *)
let target_path = 
  if Array.length Sys.argv < 3 then ( 
    print_endline "Usage: doc-runorg [inpath] [outpath]" ;
    exit (-1)) ;
  let path = Sys.argv.(2) in
  if try not (Sys.is_directory path) with _ -> false then (
    print_endline ("Fatal: " ^ path ^ " is not a directory.") ; 
    exit (-1)) ;    
  path

(** Writes a binary blob to a file at the specified path. *)
let write_binary path contents = 
  
  let is_dir path = try Sys.is_directory path with _ -> false in 
  let rec ensure_directory path =
    (* Recursion ends because 'target_path' is a directory. *)
    if is_dir path then () else 
      ( ensure_directory (Filename.dirname path) ; Unix.mkdir path 0o750 ) in
  
  let full_path = target_path // path in
  ensure_directory (Filename.dirname full_path) ;

  print_endline (path ^ " " ^ string_of_int (String.length contents) ^ " bytes") ; 

  let channel = open_out full_path in 
  output_string channel contents ;
  close_out channel 

(** Write an individual file. [write file tree site prefix] generates [file] 
    prefixed with the [site] url (such as [/docs]) and the [prefix] (such
    as [api]. Typical URL transformation is that path [{source_path}/people.md] 
    becomes path [{target_path}/api/people.md] and URL [/people.md] becomes
    URL [/docs/api/people.md]. *)
let write file files tree site prefix = 
  
  (* The full URL prefix. *)
  let url_prefix = site // prefix in
  let url_prefix_repl = "](" ^ url_prefix ^ "/" in

  (* Replaces all internal links [foo bar](/quux.md) by including the full URL
     prefix [foo bar](/docs/api/quux.md). *)
  let replace_internal_links block = 
    String.concat url_prefix_repl (String.nsplit block "](/") in

  (* Show a list of files, rendered as a table. *)
  let show_file_list = function [] -> "" | list ->
    "<table class=\"files\"><tr>"
    ^ String.concat "</tr><tr>" (List.map begin fun file ->
      "<td>" 
      ^ (match file.Read.subtitle with None -> "" | Some s -> "\n`"^s^"`\n")
      ^ "</td><td><a href=\""
      ^ (url_prefix // file.Read.path)
      ^ "\">"
      ^ file.Read.title
      ^ "</a></td>"
    end list) 
    ^ "</tr></table>"
  in
  let caption = function 
    | None -> ""
    | Some c -> "<p class=\"caption\">" ^ c ^ "</p>" in

  (* Turns an individual block into a string. *)
  let to_string elt = match elt.Read.what with 
    | `MD block -> Syntax.markdown (replace_internal_links block) 
    | `API (c,block) -> caption c ^ "<pre class=\"api\">" ^ Syntax.api block ^ "</pre>"
    | `JSON (c,block) -> caption c ^ "<pre class=\"json\">" ^ Syntax.json block ^ "</pre>"
    | `JS (c,block) -> caption c ^ "<pre class=\"js\">" ^ Syntax.js block ^ "</pre>"
    | `LIST tags -> show_file_list (Read.with_tags tags files) 
  in

  (* Generates the Jekyll header for the file. *)
  let header =
    "---\n"
    ^ "title: " ^ file.Read.title ^ "\n"
    ^ "---\n"
  in

  (* Generates the HTML menu for the file. *)
  let menu =
    let above = List.rev (Tree.ascendants file tree) in 
    let below = Tree.children file tree in 
    "<nav>"
    ^ (if above <> [] then "<ul class=\"above\"><li>" 
	^ String.concat "</li><li>" (List.map begin fun node ->  
	  "<a href=\"" ^ (url_prefix // node.Read.path) ^ "\">" 
	  ^ node.Read.title ^ "</a>"
	end above)
	^ "</li></ul>" 
      else "")
    ^ "<span class=\"active\">" 
    ^ file.Read.title ^ "</span>"
    ^ (if below <> [] then "<ul class=\"below\"><li>" 
	^ String.concat "</li><li>" (List.rev_map begin fun node ->  
	  "<a href=\"" ^ (url_prefix // node.Read.path) ^ "\">" 
	  ^ node.Read.title ^ "</a>"
	end below)
	^ "</li></ul>" 
      else "")
  ^ "</nav>"
  in

  let contents = 
    header  
    ^ "<div id=\"body\">\n"
    ^ String.concat "\n\n" (List.map to_string file.Read.body) 
    ^ "\n</div>\n"
    ^ menu
  in

  write_binary (prefix // file.Read.path) contents

(** Write a group of files with the same prefix. *)
let write_all files site prefix = 
  let tree = Tree.make files in 
  List.iter (fun file -> write file files tree site prefix) files
