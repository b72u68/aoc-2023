(* Read input file *)
let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> line :: read_lines ic
  | exception End_of_file -> []

let read_input () = open_in "input/day03.txt" |> read_lines

(* Helper functions *)
let is_number (c : char) = '0' <= c && c <= '9'
let is_dot (c : char) = c = '.'
let is_gear (c : char) = c = '*'
let is_symbol (c : char) = (not (is_number c)) && not (is_dot c)
let value_of_char (c : char) : int = int_of_char c - int_of_char '0'

let normalize_schematic lines =
  let width = String.length (List.nth lines 0) in
  let norm_row = String.make (width + 2) '.' in
  [ norm_row ] @ List.map (fun line -> "." ^ line ^ ".") lines @ [ norm_row ]

let rec get_number (line : string) (idx : int) : string option * int =
  if idx >= String.length line then (None, idx)
  else if not (is_number line.[idx]) then (None, idx + 1)
  else
    let rest, nidx = get_number line (idx + 1) in
    match rest with
    | Some n -> (Some (String.make 1 line.[idx] ^ n), nidx)
    | None -> (Some (String.make 1 line.[idx]), nidx - 1)

let get_numbers (line : string) : (int * int * int) list =
  let rec aux idx acc =
    if idx >= String.length line then acc
    else
      let num_opt, nidx = get_number line idx in
      let rest = aux nidx acc in
      match num_opt with
      | Some n -> (int_of_string n, idx, nidx - 1) :: rest
      | None -> rest
  in
  aux 0 []

let get_surrounding (p : string) (c : string) (n : string) (start_idx : int)
    (end_idx : int) : string =
  String.sub p (start_idx - 1) (end_idx - start_idx + 3)
  ^ String.make 1 c.[start_idx - 1]
  ^ String.make 1 c.[end_idx + 1]
  ^ String.sub n (start_idx - 1) (end_idx - start_idx + 3)

let is_part_number (surrounding : string) =
  String.fold_left (fun acc c -> is_symbol c || acc) false surrounding

let rec get_engine_sum = function
  | p :: c :: n :: t ->
      let numbers = get_numbers c in
      List.fold_left
        (fun acc (num, s, e) ->
          let surrounding = get_surrounding p c n s e in
          if is_part_number surrounding then acc + num else acc)
        0 numbers
      + get_engine_sum (c :: n :: t)
  | _ -> 0

let get_gears (line : string) =
  snd
    (String.fold_left
       (fun (idx, acc) c ->
         if is_gear c then (idx + 1, idx :: acc) else (idx + 1, acc))
       (0, []) line)

let get_engine_numbers engine : (string * (int * int * int) list) list =
  List.map (fun line -> (line, get_numbers line)) engine

let get_gear_part_numbers (nums : (int * int * int) list) (idx : int) :
    (int * int * int) list =
  List.filter (fun (n, s, e) -> s - 1 <= idx && idx <= e + 1) nums

let get_gear_prod (pnums : (int * int * int) list)
    (nums : (int * int * int) list) (nnums : (int * int * int) list) (idx : int)
    : int =
  let gear_nums =
    get_gear_part_numbers pnums idx
    @ get_gear_part_numbers nums idx
    @ get_gear_part_numbers nnums idx
  in
  if List.length gear_nums = 2 then
    let n1, _, _ = List.nth gear_nums 0 in
    let n2, _, _ = List.nth gear_nums 1 in
    n1 * n2
  else 0

let rec get_gear_sum = function
  | (_, pnums) :: (line, nums) :: (nline, nnums) :: t ->
      let gears = get_gears line in
      List.fold_left
        (fun acc idx -> get_gear_prod pnums nums nnums idx + acc)
        0 gears
      + get_gear_sum ((line, nums) :: (nline, nnums) :: t)
  | _ -> 0

(* Solver *)
let part_1 engine = get_engine_sum engine
let part_2 engine = get_gear_sum (get_engine_numbers engine)

let solve () =
  let engine = normalize_schematic (read_input ()) in
  Printf.printf "Part 1: %d\n" (part_1 engine);
  Printf.printf "Part 2: %d\n" (part_2 engine)
