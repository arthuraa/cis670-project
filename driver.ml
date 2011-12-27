#use "/Users/bob/.ocamlinit";;
#directory "_build";;
#load "util.cmo";;
#load "fc_p.cmo";;
#load "fc_l.cmo";;

module X = Fc_l;;
open Fc_p
open Fc_l 
let string_of_file file =
  File.lines_of file |> Enum.fold (fun x y -> x ^ y ^ "\n") "";;

let x = string_of_file "test/1.ml" 
let ts = Fc_l.tokens x 


