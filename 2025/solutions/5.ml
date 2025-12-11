let input = String.trim {|
#include "5"
|} in

let ranges, ids =
  match String.split_on_char 'x' input |> List.map String.trim with
  | [ r; i ] ->
      ( String.split_on_char '\n' r
        |> List.map (fun l ->
            match String.split_on_char '-' l with
            | [ a; b ] -> (int_of_string a, int_of_string b)
            | _ -> assert false),
        String.split_on_char '\n' i |> List.map int_of_string )
  | _ -> assert false
in

let p1 ranges ids =
  ids
  |> List.map (fun id ->
      if List.exists (fun (a, b) -> a <= id && id <= b) ranges then 1 else 0)
  |> List.fold_left ( + ) 0
in

let p2 ranges =
  let rec insert (lo, hi) = function
    | [] -> [ (lo, hi) ]
    | (a, b) :: rest ->
        if hi < a - 1 then (lo, hi) :: (a, b) :: rest
        else if lo > b + 1 then (a, b) :: insert (lo, hi) rest
        else insert (min lo a, max hi b) rest
  in
  ranges
  |> List.fold_left (fun acc r -> insert r acc) []
  |> List.fold_left (fun acc (a, b) -> acc + abs (a - b) + 1) 0
in

let p1 = p1 ranges ids in
let p2 = p2 ranges in
Printf.printf "%d, %d\n" p1 p2
