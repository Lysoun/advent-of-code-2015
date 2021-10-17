open Printf
open List
open String

let input = "input.txt"

let compute_wrap_feets_of_smallest_dimensions a b = a + a + b + b;;

let compute_wrap_feets l w h =
    if l >= w then
        if w >= h then
            (compute_wrap_feets_of_smallest_dimensions h w)
        else
            if h >= l then
                (compute_wrap_feets_of_smallest_dimensions l w)
            else
                (compute_wrap_feets_of_smallest_dimensions h w)
    else if h >= w then
            (compute_wrap_feets_of_smallest_dimensions w l)
         else
            (compute_wrap_feets_of_smallest_dimensions h l)
;;

let compute_right_rectangular_prism_square_feet l w h = (compute_wrap_feets l w h) + l * w * h;;

let compute_line_square_feet line =
    let values = (List.map (int_of_string) (split_on_char 'x' line)) in
    (compute_right_rectangular_prism_square_feet (nth values 0) (nth values 1) (nth values 2))
    ;;

let rec input_lines file sum =
   match try (input_line file) with End_of_file -> "" with
      | "" -> sum
      | line -> (input_lines file (sum + (compute_line_square_feet line)))
    ;;

print_int (input_lines (open_in input) 0);;