(* Read input file *)
type card = {
  id : int;
  win_cards : int list;
  own_cards : int list;
  win_count : int;
  copy_count : int;
  score : int;
}

let is_number (c : char) = '0' <= c && c <= '9'
let value_of_char (c : char) = int_of_char c - int_of_char '0'

let get_game_id (line : string) =
  let rec aux idx num =
    if idx >= String.length line then num
    else
      let c = line.[idx] in
      if is_number c then aux (idx + 1) ((num * 10) + value_of_char c)
      else aux (idx + 1) num
  in
  aux 0 0

let get_cards (cards_str : string) =
  cards_str |> String.split_on_char ' '
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map int_of_string |> List.sort compare

let process_line (line : string) =
  let processed = String.split_on_char ':' line in
  let id = get_game_id (List.nth processed 0) in
  let cards_str = List.nth processed 1 in
  let win_own_str = String.split_on_char '|' cards_str in
  let win_str = String.trim (List.nth win_own_str 0) in
  let own_str = String.trim (List.nth win_own_str 1) in
  let win_cards = get_cards win_str in
  let own_cards = get_cards own_str in
  { id; win_cards; own_cards; win_count = 0; copy_count = 1; score = 0 }

let rec read_lines (ic : in_channel) =
  match input_line ic with
  | line -> line :: read_lines ic
  | exception End_of_file -> []

let read_input () = open_in "input/day04.txt" |> read_lines

(* Helper functions *)
let update_score_and_win_count (card : card) =
  let rec aux card = function
    | [], _ | _, [] -> card
    | w :: wt, o :: ot ->
        if w = o then
          if card.score = 0 then
            aux { card with win_count = 1; score = 1 } (wt, ot)
          else
            aux
              {
                card with
                win_count = card.win_count + 1;
                score = card.score * 2;
              }
              (wt, ot)
        else if w < o then aux card (wt, o :: ot)
        else aux card (w :: wt, ot)
  in
  aux card (card.win_cards, card.own_cards)

let rec update_copy_count (cards : card list) =
  match cards with
  | [] -> []
  | c :: t ->
      let t = update_copy_count_rec c t in
      c :: update_copy_count t

and update_copy_count_rec (card : card) (rest_cards : card list) =
  let rec aux = function
    | 0, t -> t
    | _, [] -> []
    | n, c :: t ->
        { c with copy_count = c.copy_count + card.copy_count } :: aux (n - 1, t)
  in
  aux (card.win_count, rest_cards)

(* Solver *)
let part_1 cards = List.fold_left (fun acc card -> card.score + acc) 0 cards

let part_2 cards =
  List.fold_left
    (fun acc card -> card.copy_count + acc)
    0 (update_copy_count cards)

let solve () =
  let cards =
    read_input () |> List.map process_line
    |> List.map update_score_and_win_count
  in
  Printf.printf "Part 1: %d\n" (part_1 cards);
  Printf.printf "Part 2: %d\n" (part_2 cards)
