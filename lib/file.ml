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
