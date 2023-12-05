(* Read input file *)
let rec read_lines (ic : in_channel) : string list =
  match input_line ic with
  | line -> line :: read_lines ic
  | exception End_of_file -> []

let read_input () : string list = open_in "input/day01.txt" |> read_lines

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
let is_numeric (c : char) = '0' <= c && c <= '9'

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
  String.fold_left
    (fun (idx, digits) c ->
      let nidx = idx + 1 in
      if is_numeric c then (nidx, int_value c :: digits)
      else if is_part_2 then
        match get_digit_string line idx with
        | Some d -> (nidx, d :: digits)
        | None -> (nidx + 1, digits)
      else (nidx, digits))
    (0, []) line
  |> snd |> List.rev

(* Solver *)
let get_calibration (lines : string list) (is_part_2 : bool) =
  List.fold_left
    (fun acc line ->
      let digits = get_digits line is_part_2 in
      try
        let d1 = List.nth digits 0 in
        let d2 = List.nth digits (List.length digits - 1) in
        (d1 * 10) + d2 + acc
      with _ -> acc)
    0 lines

let part_1 lines = get_calibration lines false
let part_2 lines = get_calibration lines true

let solve () =
  let lines = read_input () in
  Printf.printf "Part 1: %d\n" (part_1 lines);
  Printf.printf "Part 2: %d\n" (part_2 lines)
