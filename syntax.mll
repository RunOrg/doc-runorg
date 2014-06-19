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

{

  let api block =
    let lexbuf = Lexing.from_string block in 
    let buffer = Buffer.create (String.length block) in
    apiP true buffer lexbuf ;
    Buffer.contents buffer 

  let json block =
    let lexbuf = Lexing.from_string block in 
    let buffer = Buffer.create (String.length block) in
    jsonP true buffer lexbuf ;
    Buffer.contents buffer 


} 
