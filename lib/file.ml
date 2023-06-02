let read_lines f =
  let input = open_in f in
  let rec add_line acc =
    match input_line input with
    | line -> add_line (line :: acc)
    | exception End_of_file ->
      close_in input;
      CCList.rev acc
  in
  add_line []

let read_all f =
  let input = open_in f in
  let rec add_char acc =
    match input_char input with
    | char -> add_char (char :: acc)
    | exception End_of_file ->
      close_in input;
      CCList.rev acc
  in
  add_char [] |> CCString.of_list

let last s = CCString.get s (CCString.length s - 1)
let has_final_newline s = last s = '\n'

let ensure_final_newline s =
  if has_final_newline s then
    s
  else
    s ^ "\n"

let write_all f s =
  let output = open_out f in
  Printf.fprintf output "%s" s;
  close_out output
