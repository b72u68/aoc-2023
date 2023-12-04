(* Read input file *)
type set = { red : int; blue : int; green : int }
type game = { id : int; sets : set list }

let process_cube_str (str : string) : string * int =
  let processed = String.split_on_char ' ' (String.trim str) in
  let cube = List.nth processed 1 in
  let count = int_of_string (List.nth processed 0) in
  (cube, count)

let process_set_str (str : string) : set =
  let set_str_list = String.split_on_char ',' str in
  List.fold_left
    (fun acc set_str ->
      let cubes_str = String.split_on_char ',' set_str in
      let cubes = List.map process_cube_str cubes_str in
      List.fold_right
        (fun cube acc ->
          match cube with
          | "red", n -> { acc with red = acc.red + n }
          | "blue", n -> { acc with blue = acc.blue + n }
          | "green", n -> { acc with green = acc.green + n }
          | _ -> failwith "Invalid cube")
        cubes acc)
    { red = 0; blue = 0; green = 0 }
    set_str_list

let process_line (line : string) : game =
  let processed = String.split_on_char ':' line in
  let id =
    int_of_string (List.nth (String.split_on_char ' ' (List.nth processed 0)) 1)
  in
  let sets =
    List.map process_set_str (String.split_on_char ';' (List.nth processed 1))
  in
  { id; sets }

let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> process_line line :: read_lines ic
  | exception End_of_file -> []

let read_input () = open_in "input/day02.txt" |> read_lines

(* Helper functions *)
let string_of_set = function
  | { red; blue; green } ->
      Printf.sprintf "red: %d, blue: %d, green: %d" red blue green

let string_of_game = function
  | { id; sets } ->
      Printf.sprintf "%d: %s" id
        (String.concat "; " (List.map string_of_set sets))

let is_set_possible = function
  | { red; blue; green } -> red <= 12 && blue <= 14 && green <= 13

let is_game_possible = function
  | { id; sets } ->
      List.fold_left (fun acc set -> is_set_possible set && acc) true sets

let get_possible_set = function
  | { id; sets } ->
      List.fold_left
        (fun acc set ->
          match set with
          | { red; blue; green } ->
              {
                red = max red acc.red;
                blue = max blue acc.blue;
                green = max green acc.green;
              })
        { red = 0; blue = 0; green = 0 }
        sets

let get_power = function { red; blue; green } -> red * blue * green

(* Solver *)
let part_1 games =
  List.fold_left
    (fun acc game -> if is_game_possible game then game.id + acc else acc)
    0 games

let part_2 games =
  List.fold_left
    (fun acc game ->
      let set = get_possible_set game in
      let power = get_power set in
      power + acc)
    0 games

let solve () =
  let games = read_input () in
  Printf.printf "Part 1: %d\n" (part_1 games);
  Printf.printf "Part 2: %d\n" (part_2 games)
