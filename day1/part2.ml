open Printf
open String

let input = "input.txt"

let rec string_to_char_list ch = match ch with
    | "" -> []
    | ch -> (String.get ch 0 ) :: (string_to_char_list (String.sub ch 1 ( (String.length ch)-1) ) )  ;;

let move character = match character with
    | '(' -> 1
    | _ -> -1
;;

let rec compute_floor str floor pos = match str with
    | [] -> pos
    | _ when floor = -1 -> pos
    | h::t -> compute_floor t (floor + (move h)) (pos + 1)
;;


print_int (compute_floor (string_to_char_list (input_line (open_in input))) 0 0);;