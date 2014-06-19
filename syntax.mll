{ (* © 2014 RunOrg *)
  
  let span buffer cls content = 
    List.iter (Buffer.add_string buffer) 
      [ "<span class=\"" ; cls ; "\">" ; content ; "</span>" ]

}

rule apiP sol buffer = parse 
| "  " { if not sol then Buffer.add_string buffer "  " ; apiP false buffer lexbuf }
| [ ^ '\n' ' ' ] * as s { Buffer.add_string buffer s ; apiP false buffer lexbuf }
| _ as c { Buffer.add_char buffer c ; apiP (c = '\n') buffer lexbuf }
| eof { () } 

and jsonP sol buffer = parse 
| "  " { if not sol then Buffer.add_string buffer "  " ; jsonP false buffer lexbuf }
| [ ^ '\n' ' ' ] * as s { Buffer.add_string buffer s ; jsonP false buffer lexbuf }
| _ as c { Buffer.add_char buffer c ; jsonP (c = '\n') buffer lexbuf }
| eof { () } 

and jsP sol buffer = parse 
| "  " { if not sol then Buffer.add_string buffer "  " ; jsP false buffer lexbuf }
| [ ^ '\n' ' ' ] * as s { Buffer.add_string buffer s ; jsP false buffer lexbuf }
| _ as c { Buffer.add_char buffer c ; jsP (c = '\n') buffer lexbuf }
| eof { () } 

and markdownP stack buffer = parse 
| '`' {
  let stack = match stack with 
    | `CODE :: t -> Buffer.add_string buffer "</code>" ; t 
    | _ -> Buffer.add_string buffer "<code>" ; `CODE :: stack in
  markdownP stack buffer lexbuf
}
| "__" {
  let stack = match stack with 
    | `I :: t -> Buffer.add_string buffer "</i>" ; t 
    | _ -> Buffer.add_string buffer "<i>" ; `I :: stack in
  markdownP stack buffer lexbuf
}
| "**" {
  let stack = match stack with 
    | `B :: t -> Buffer.add_string buffer "</b>" ; t 
    | _ -> Buffer.add_string buffer "<b>" ; `B :: stack in
  markdownP stack buffer lexbuf
}
| "[" {
  let buf2  = Buffer.create 100 in
  let inner = markdownP [ `LINK ] buf2 lexbuf in
  if inner = [] then begin
    Buffer.add_string buffer "<a href=\"" ;
    markdownP (`A (Buffer.contents buf2) :: stack) buffer lexbuf 
  end else begin
    Buffer.add_char buffer '[' ;
    Buffer.add_string buffer (Buffer.contents buf2) ;
    inner
  end
}
| "](" {
  match stack with 
  | [ `LINK ] -> []
  | other -> Buffer.add_string buffer "](" ; markdownP stack buffer lexbuf
}
| ')' {
  match stack with 
  | `A body :: t -> Buffer.add_string buffer "\">" ;
                    Buffer.add_string buffer body ;
		    Buffer.add_string buffer "</a>" ; 
		    markdownP t buffer lexbuf 
  | _ -> Buffer.add_char buffer ')' ;
         markdownP stack buffer lexbuf
}
| [^ '&' '<' '"' ')' '>' ']' '[' '*' '_' '`' ] + as s { Buffer.add_string buffer s ; markdownP stack buffer lexbuf }
| '&' { Buffer.add_string buffer "&amp;" ; markdownP stack buffer lexbuf }
| '<' { Buffer.add_string buffer "&lt;" ; markdownP stack buffer lexbuf }
| '"' { Buffer.add_string buffer "&quot;" ; markdownP stack buffer lexbuf }
| '>' { Buffer.add_string buffer "&gt;" ; markdownP stack buffer lexbuf }
| _ as c { Buffer.add_char buffer c ; markdownP stack buffer lexbuf }
| eof { stack }

{

  let process f block = 
    let lexbuf = Lexing.from_string block in 
    let buffer = Buffer.create (String.length block) in
    f buffer lexbuf ;
    Buffer.contents buffer 
    
  let api block =
    process (apiP true) block 

  let json block =
    process (jsonP true) block

  let js block =
    process (jsP true) block

(** This is not really a markdown processor: it does not handle 
    the full range of markdown semantics. 

    It does handle: 
     - newlines to split paragraphs
     - hyphens (-) to define lists
     - hashes (#) to define headings
     - **stars** to highlight in bold
     - [text](url) for links
     - __underscores__ to highlight in italics
     - `ticks` to enter code

    It does not handle: 
     - nested lists
     - ordered lists
     - blockquotes
     - code blocks
     - [text][ref] for footnote links
*)

  let markdown block = 

    let lines = List.map BatString.trim (BatString.nsplit block "\n") in

    (* Each block is a string containing text matching a specific semantic block,
       such as '## Heading' or 'paragraph' or '- list item' *)
    let rec to_blocks acc list = 
      let newp acc = match acc with [] :: _ -> acc | l -> [] :: l in
      let add  acc x = match acc with [] -> [[x]] | h :: t -> (x :: h) :: t in
      match list with 
      | [] -> List.rev_map (fun lines -> String.concat " " (List.rev lines)) acc 
      | "" :: t -> to_blocks (newp acc) t 
      | line :: t when line.[0] = '#' || line.[0] = '-' -> to_blocks (add (newp acc) line) t
      | line :: t -> to_blocks (add acc line) t  
    in

    let blocks = to_blocks [] lines in 

    (* Traverse all blocks, outputting HTML markup to the buffer. *)
    let buffer = Buffer.create (String.length block * 2) in
    let parse_block block = 
      let lexbuf = Lexing.from_string block in 
      ignore (markdownP [] buffer lexbuf)
    in
    
    let in_list = List.fold_left (fun in_list block -> 
      print_endline ("Block: >>" ^ block ^ "<<") ;
      if block = "" then 
	in_list
      else if block.[0] = '#' then begin

	let level = 
	  let i = ref 1 in 
	  while !i < String.length block && block.[!i] = '#' do incr i done ;
	  !i in

	let nlevel = string_of_int level in 

	if in_list then Buffer.add_string buffer "</ul>" ;
	Buffer.add_string buffer "<h" ; 
	Buffer.add_string buffer nlevel ;
	Buffer.add_char buffer '>' ;
	
	parse_block (String.sub block level (String.length block - level)) ;

	Buffer.add_string buffer "</h" ; 
	Buffer.add_string buffer nlevel ;
	Buffer.add_char buffer '>' ;

	false

      end else if block.[0] = '-' then begin
	
	Buffer.add_string buffer (if in_list then "<li>" else "<ul><li>") ; 
	
	parse_block (BatString.lchop block) ;

	Buffer.add_string buffer "</li>" ;

	true 

      end else begin

	if in_list then Buffer.add_string buffer "</ul>" ;
	Buffer.add_string buffer "<p>" ;
	
	parse_block block ;

	Buffer.add_string buffer "</p>" ;

	false 

      end) false blocks 
    in

    if in_list then Buffer.add_string buffer "</ul>" ;

    Buffer.contents buffer 
} 
