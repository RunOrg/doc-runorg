open Explore
open Tree
open Write

let files = Explore.all_data

let js = Read.only `JS files 
let () = Write.write_all files "/docs" "js"

let api = Read.only `API files 
let () = Write.write_all files "/docs" "api"
