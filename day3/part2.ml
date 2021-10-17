open Printf
open Hashtbl

let input = "input.txt"

let rec string_to_char_list ch = match ch with
    | "" -> []
    | _ when ((String.length ch) < 2) -> [(String.get ch 0)]
    | ch -> (String.get ch 0) :: (string_to_char_list (String.sub ch 2 ( (String.length ch)-2) ) )  ;;

let move direction (x, y) = match direction with
    | 'v' -> (x, y - 1)
    | '^' -> (x, y + 1)
    | '>' -> (x + 1, y)
    | _ ->  (x - 1, y)
;;

let deliverPresent houses position =
    if (Hashtbl.mem houses position) then
            (Hashtbl.replace houses position ((Hashtbl.find houses position) + 1))
        else
            (Hashtbl.add houses position 1)
;;

let rec deliverPresents instructions houses currentPosition = match instructions with
    | [] -> ()
    | h::t ->
    let newPosition = (move h currentPosition) in
    (deliverPresent houses newPosition);
    (deliverPresents t houses newPosition)
;;

let instructions = (input_line (open_in input));;
let houses = (Hashtbl.create 1);;
(Hashtbl.add houses (0, 0) 1);;

(deliverPresents (string_to_char_list instructions) houses (0, 0));;
(deliverPresents (string_to_char_list (String.sub instructions 1 (String.length instructions - 1))) houses (0, 0));;

print_int (Hashtbl.length houses);;