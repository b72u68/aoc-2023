(* Read input file *)
let read_input () : string list =
  let ic = open_in "input/day01.txt" in
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done;
    !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines

(* Helper functions *)
type number_info = { value : int; str : string }

let number_record : number_info list =
  [
    { value = 1; str = "one" };
    { value = 2; str = "two" };
    { value = 3; str = "three" };
    { value = 4; str = "four" };
    { value = 5; str = "five" };
    { value = 6; str = "six" };
    { value = 7; str = "seven" };
    { value = 8; str = "eight" };
    { value = 9; str = "nine" };
  ]

let int_value (c : char) = int_of_char c - int_of_char '0'
let is_numeric (c : char) = match c with '0' .. '9' -> true | _ -> false

let get_digit_string (line : string) (idx : int) : int option =
  let num_opt =
    List.find_opt
      (fun num_info ->
        if line.[idx] = num_info.str.[0] then
          try String.sub line idx (String.length num_info.str) = num_info.str
          with _ -> false
        else false)
      number_record
  in
  match num_opt with Some num_rec -> Some num_rec.value | None -> None

let get_digits (line : string) (is_part_2 : bool) : int list =
  let rec aux (idx : int) (acc : int list) =
    if idx >= String.length line then acc
    else
      let rest = aux (idx + 1) acc in
      if is_numeric line.[idx] then int_value line.[idx] :: rest
      else if is_part_2 then
        match get_digit_string line idx with
        | Some d -> d :: rest
        | None -> rest
      else rest
  in
  aux 0 []

(* Process input *)
let lines = read_input ()

(* Solver *)
let part_1 () =
  List.fold_left
    (fun acc line ->
      let digits = get_digits line false in
      try
        let d1 = List.nth digits 0 in
        let d2 = List.nth digits (List.length digits - 1) in
        (d1 * 10) + d2 + acc
      with _ -> acc)
    0 lines

let part_2 () =
  List.fold_left
    (fun acc line ->
      let digits = get_digits line true in
      try
        let d1 = List.nth digits 0 in
        let d2 = List.nth digits (List.length digits - 1) in
        (d1 * 10) + d2 + acc
      with _ -> acc)
    0 lines

let solve () =
  Printf.printf "Part 1: %d\n" (part_1 ());
  Printf.printf "Part 2: %d\n" (part_2 ())
