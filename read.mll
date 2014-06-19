{ (* © 2014 RunOrg *)
  
  module Map = BatMap

  type tag = {
    tag  : string ; 
    args : (string,string) Map.t ;
    content : string option ; 
  }

  let rest lexbuf = 
    String.sub lexbuf.Lexing.lex_buffer
      (lexbuf.Lexing.lex_start_pos - 1)
      (lexbuf.Lexing.lex_buffer_len + 1)
}

let wsp = [ ' ' '\t' '\n' ] *
let id = [ 'a' - 'z' 'A' - 'Z' ][ 'a' - 'z' 'A' - 'Z'  '0' - '9' ] *

rule tagP tags = parse
  | wsp { tagP tags lexbuf }
  | '<' (id as tag) { argsP tag Map.empty tags lexbuf }
  | _ { failwith ("Parse error: expecting tag, found:\n" ^ (rest lexbuf)) }
  | eof { List.rev tags }

and argsP tag args tags = parse
  | wsp (id as key) '=' '"' ([^ '"' ]* as value) '"' { argsP tag (Map.add key value args) tags lexbuf }
  | wsp "/>" { tagP ({ tag ; args ; content = None } :: tags) lexbuf }
  | wsp '>' { bodyP tag args (Buffer.create 100) tags lexbuf }
  | _ { failwith ("Parse error: expected argument or tag end, found:\n" ^ (rest lexbuf)) }
  | eof { failwith "Parse error: expected argument or tag end, found EOF." }

and bodyP tag args buffer tags = parse
  | [ ^ '<' ] * as content { Buffer.add_string buffer content ; bodyP tag args buffer tags lexbuf }
  | "</" (id as close) '>' { 
    if tag = close then tagP ({ tag ; args ; content = Some (Buffer.contents buffer) } :: tags) lexbuf
    else ( Buffer.add_string buffer ("</" ^ close ^ ">") ; bodyP tag args buffer tags lexbuf ) }
  | _ as c { Buffer.add_char buffer c ; bodyP tag args buffer tags lexbuf }
  | eof { failwith "Parse error: expected closing tag, found EOF." }

{

  type element = {
    where : [ `ANY | `API | `JS ] ;
    what  :
      (* A piece of markdown *) 
      [ `MD of string 
      (* A request or response example. *)
      | `API of string option * string 
      (* A JSON example. *)
      | `JSON of string option * string
      (* A JS example *)
      | `JS of string option * string ] ;
  }

  (** An individual source file. *)
  type t = {

    (** The path of the source file. 
       Will be mirrored for the output file. *)
    path   : string ; 

    (** The title of the page (language-independent). *)
    title  : string ;

    (** The subtitle. [None] by default, but set to either [js] or
	[api] when in the corresponding section. *)
    subtitle : string option ; 

    (** The subtitle, in the JS section. *)
    js     : string option ;

    (** The subtitle, in the API section. *)
    api    : string option ;

    (** Tags associated to the page. *)
    tags   : string list ;

    (** The parent page in the hierarchy. *)
    parent : string option ; 

    (** The rendered elements. *)
    body   : element list ; 
  }

  let parse fullpath path = 
    let channel  = open_in fullpath in 
    let lexbuf   = Lexing.from_channel channel in 
    let elements = try tagP [] lexbuf with Failure s -> failwith ("In file " ^ path ^ ":\n" ^ s) in

    let title, parent, tags, js, api = 
      try let tag = List.find (fun t -> t.tag = "page") elements in 
	  let title = try Map.find "title" tag.args with Not_found -> "" in
	  let parent = try Some (Map.find "parent" tag.args) with Not_found -> None in 
	  let tags = BatString.nsplit (try Map.find "tags" tag.args with Not_found -> "") " " in
	  let js = try Some (Map.find "js" tag.args) with Not_found -> None in 
	  let api = try Some (Map.find "api" tag.args) with Not_found -> None in 
	  title, parent, tags, js, api
      with Not_found -> "", None, [], None, None 
    in

    let body = List.concat (List.map begin fun tag -> 
      match tag.tag, tag.content with 

      | "doc", Some body -> 	
	
	let filter = try Map.find "for" tag.args with Not_found -> "all" in
	let where  = match filter with 
	  | "js" -> `JS
	  | "api" -> `API 
	  | _ -> `ANY in
	[ { where ; what = `MD body } ]

      | "example", Some body -> 
	let title = try Some (Map.find "caption" tag.args) with Not_found -> None in
	let kind = try Map.find "type" tag.args with Not_found -> "json" in
	if kind = "api" then [ { where = `API ; what = `API (title, body) } ] else
	  if kind = "json" then [ { where = `ANY ; what = `JSON (title, body) } ] else
	    if kind = "js" then [ { where = `JS ; what = `JS (title, body) } ] else
	      []

      | _ -> []

    end elements) in
    
    { path ; title ; js ; api ; parent ; body ; tags ; subtitle = None }

  let only what files = 

    let is_content elt = match elt.what with `MD _ | `API _ | `JSON _ | `JS _ -> true in 
    let has_content file = List.exists is_content file.body in

    let is_kept elt = match elt.where with 
      | `ANY -> true 
      | `JS  -> what = `JS 
      | `API -> what = `API in

    let filter file = { 
      file with 
	subtitle = (match what with `JS -> file.js | `API -> file.api) ;
	body = List.filter is_kept file.body 
    } in

    List.filter has_content (List.map filter files) 

} 
