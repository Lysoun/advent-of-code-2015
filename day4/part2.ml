open Printf
open Digest

let input = "input.txt"

let secretKey = (input_line (open_in input));;

let rec find_lowest_md5_starting_with_five_zeroes i =
    let md5 = (Digest.to_hex (Digest.string (String.concat "" [secretKey; (string_of_int i)]))) in
    if (String.sub md5 0 6) = "000000"
        then i
        else (find_lowest_md5_starting_with_five_zeroes (i + 1))
;;

print_int (find_lowest_md5_starting_with_five_zeroes 0);;