let input = String.trim {|
#include "9"
|} in

let points =
  input |> String.split_on_char '\n'
  |> List.map (fun l ->
      match String.split_on_char ',' l |> List.map int_of_string with
      | [ x; y ] -> (x, y)
      | _ -> failwith "invalid point format")
  |> Array.of_list
in

let n = Array.length points in

let edges =
  Array.init n (fun i ->
      let x1, y1 = points.(i) in
      let x2, y2 = points.((i + 1) mod n) in
      (min x1 x2, min y1 y2, max x1 x2, max y1 y2))
in

Array.fast_sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) edges;

let contained min_x min_y max_x max_y =
  let rec loop i =
    if i >= n then true
    else
      let edge_min_x, edge_min_y, edge_max_x, edge_max_y = edges.(i) in
      if edge_min_x >= max_x then true
      else if edge_max_x <= min_x then loop (i + 1)
      else if max_y > edge_min_y && min_y < edge_max_y then false
      else loop (i + 1)
  in
  loop 0
in

let p1 = ref 0 in
let p2 = ref 0 in

for i = 0 to n - 1 do
  let x1, y1 = points.(i) in
  for j = i + 1 to n - 1 do
    let x2, y2 = points.(j) in
    let dx = x2 - x1 in
    let dy = y2 - y1 in
    if dx <> 0 && dy <> 0 then (
      let area = (abs dx + 1) * (abs dy + 1) in
      if area > !p1 then p1 := area;
      if area > !p2 then
        let min_x = if dx > 0 then x1 else x2 in
        let min_y = if dy > 0 then y1 else y2 in
        let max_x = if dx > 0 then x2 else x1 in
        let max_y = if dy > 0 then y2 else y1 in
        if contained min_x min_y max_x max_y then p2 := area)
  done
done;

Printf.printf "%d, %d\n" !p1 !p2
