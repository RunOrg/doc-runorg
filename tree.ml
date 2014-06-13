(* © 2014 RunOrg *)

module Map = BatMap
open Read

(** Generates a tree representation for a list of parsed files, based on their 
    [parent] property. *)

(** A tree, includes data structures for the relevant queries. *)
type t = {
  
  (** Key is path, value is the file at that path. *)
  all : (string, Read.t) Map.t ;  

  (** Key is path, value is the files that have that path as a parent. *)
  children: (string option, Read.t list) Map.t ;

}

(** Create a tree from a source list. *)
let make files = { 
  all = List.fold_left (fun map file -> Map.add file.path file map) Map.empty files ;
  children = List.fold_left (fun map file -> 
      let current = try Map.find file.parent map with Not_found -> [] in
      Map.add file.parent (file :: current) map) Map.empty files ;
}

(** Grab the direct children of a file. *)
let children file t = 
  try Map.find (Some file.path) t.children with Not_found -> []

(** Grab all the ascendants of a file all up to a root, in reverse order,
    excluding the file itself. *)
let rec ascendants file t = 
  match file.parent with None -> [] | Some id ->
    let parent = try Map.find id t.all with Not_found -> 
      failwith (Printf.sprintf ("File '%s' references parent '%s' which does not exist") file.path id) in
    parent :: ascendants parent t

