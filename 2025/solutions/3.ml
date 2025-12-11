let input = String.trim {|
#include "3"
|} in

let chars = [| '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; '0' |] in

let rec solve_general str depth acc =
  if depth = 0 then acc
  else
    let search_len = String.length str - depth + 1 in
    match
      Array.find_map
        (fun c ->
          match String.index_opt (String.sub str 0 search_len) c with
          | None -> None
          | Some idx ->
              Some (c, String.sub str (idx + 1) (String.length str - idx - 1)))
        chars
    with
    | None -> assert false
    | Some (found_char, after) ->
        solve_general after (depth - 1) (found_char :: acc)
in

let solve str depth =
  String.split_on_char '\n' str
  |> List.map (fun l -> solve_general l depth [])
  |> List.map (fun l ->
      List.fold_right (fun c acc -> acc ^ String.make 1 c) l "")
  |> List.map int_of_string |> List.fold_left ( + ) 0
in

Printf.printf "%d, %d\n" (solve input 2) (solve input 12)
