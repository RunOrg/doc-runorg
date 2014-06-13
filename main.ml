open Explore
open Tree
open Write

let files = Explore.all_data

let js = Read.only `JS files 
let () = Write.write_all js "/docs" "js"

let api = Read.only `API files 
let () = Write.write_all api "/docs" "api"
