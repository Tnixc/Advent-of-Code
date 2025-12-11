let input = String.trim {|
#include "7"
|} in

let grid =
  input |> String.split_on_char '\n' |> List.map String.to_seq
  |> List.map Array.of_seq |> Array.of_list
in

let height = Array.length grid in
let width = Array.length grid.(0) in

let hist = Hashtbl.create 7000 in
let p1 = ref 0 in
let rec p2 (y, x) =
  match Hashtbl.find_opt hist (y, x) with
  | Some v -> v
  | None ->
      let count =
        if y >= height - 1 then 1
        else if grid.(y + 1).(x) = '^' then (
          incr p1;
          let left = if x > 0 then p2 (y + 1, x - 1) else 0 in
          let right = if x < width - 1 then p2 (y + 1, x + 1) else 0 in
          left + right)
        else p2 (y + 1, x)
      in
      Hashtbl.add hist (y, x) count;
      count
in

let p2 =
  (0, grid.(0) |> Array.find_index (fun c -> c == 'S') |> Option.get) |> p2
in

Printf.printf "%d, %d\n" !p1 p2
