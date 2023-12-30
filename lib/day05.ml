(* Read input file *)
type map = { src : int; dest : int; range : int }

let string_of_map map =
  Printf.sprintf "%d -> %d (%d)" map.src map.dest map.range

let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> line :: read_lines ic
  | exception End_of_file -> []

let process_map_line (line : string) =
  let num_strs = String.split_on_char ' ' line in
  {
    dest = int_of_string (List.nth num_strs 0);
    src = int_of_string (List.nth num_strs 1);
    range = int_of_string (List.nth num_strs 2);
  }

let process_lines (lines : string list) =
  let seeds_line =
    String.trim (List.nth (String.split_on_char ':' (List.hd lines)) 1)
  in
  let seeds = List.map int_of_string (String.split_on_char ' ' seeds_line) in
  let maps =
    fst
      (List.fold_left
         (fun (maps, skip) line ->
           if skip then (maps, false)
           else if line = "" then ([] :: maps, true)
           else
             let map = process_map_line line in
             ((map :: List.hd maps) :: List.tl maps, false))
         ([ [] ], true)
         (List.tl (List.tl lines)))
  in
  let sorted_maps =
    List.rev_map (List.sort (fun m1 m2 -> compare m1.src m2.src)) maps
  in
  (seeds, sorted_maps)

let read_input () = open_in "input/test05.txt" |> read_lines |> process_lines

(* Helper functions *)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let rec get_next_value (map : map list) (value : int) =
  match map with
  | [] -> value
  | m :: t ->
      if value < m.src || m.src + m.range <= value then get_next_value t value
      else value - m.src + m.dest

let get_location (maps : map list list) (seed : int) =
  List.fold_left (fun acc m -> get_next_value m acc) seed maps

let rec get_seeds = function
  | [] -> []
  | [ s ] -> [ s ]
  | s :: r :: t -> (s -- (s + r)) @ get_seeds t

let uniq_cons x xs = if List.mem x xs then xs else x :: xs
let remove_dup xs = List.fold_right uniq_cons xs []

(* Solver *)
let part_1 (seeds, maps) =
  let locations = List.map (get_location maps) seeds in
  List.fold_left min (List.hd locations) locations

let part_2 (seed_ranges, maps) =
  let locations =
    List.map (get_location maps) (remove_dup (get_seeds seed_ranges))
  in
  List.fold_left min (List.hd locations) locations

let solve () =
  let lines = read_input () in
  Printf.printf "Part 1: %d\n" (part_1 lines);
  Printf.printf "Part 2: %d\n" (part_2 lines)
