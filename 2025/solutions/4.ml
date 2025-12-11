let input = String.trim {|
#include "4"
|} in

let solve str =
  let grid_init =
    String.split_on_char '\n' str
    |> List.map (fun s -> Array.init (String.length s) (fun i -> s.[i]))
    |> Array.of_list
  in
  let height = Array.length grid_init in
  let width = Array.length grid_init.(0) in

  let p1 = ref 0 in

  let count_neighbors grid y x =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
    |> List.filter (fun (dx, dy) ->
        let ny = y + dy in
        let nx = x + dx in
        ny >= 0 && ny < height && nx >= 0 && nx < width
        &&
        let c = grid.(ny).(nx) in
        c = '@' || c = 'X')
    |> List.length
  in

  let grid = grid_init in

  let step1 () =
    let grid_temp = Array.map Array.copy grid in
    let count = ref 0 in

    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if grid.(y).(x) = '@' then
          if count_neighbors grid y x < 4 then (
            grid_temp.(y).(x) <- '.';
            incr count)
          else grid_temp.(y).(x) <- '@'
        else grid_temp.(y).(x) <- grid.(y).(x)
      done
    done;

    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        grid.(y).(x) <- grid_temp.(y).(x)
      done
    done;

    !count
  in

  let step () =
    let count = ref 0 in

    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if grid.(y).(x) = '@' then
          if count_neighbors grid y x < 4 then (
            grid.(y).(x) <- '.';
            incr count)
      done
    done;

    !count
  in

  let rec loop total_count depth =
    let count = if depth = 0 then step1 () else step () in
    if depth = 0 then p1 := count;
    if count = 0 then total_count
    else loop (total_count + count) (depth + 1)
  in

  let count = loop 0 0 in
  (!p1, count)
in

let p1, p2 = solve input in
Printf.printf "%d, %d\n" p1 p2
