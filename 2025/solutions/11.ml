let input = String.trim {|
#include "11"
|} in

let graph = Hashtbl.create 600 in
Hashtbl.add graph "out" [];

input |> String.split_on_char '\n'
|> List.iter (fun line ->
    match String.split_on_char ':' line with
    | [ node; edges ] ->
        let edge = edges |> String.split_on_char ' ' in
        Hashtbl.add graph node edge
    | _ -> ());

let toposorted =
  let sorted_nodes = ref [] in
  let marked_nodes = Hashtbl.create 600 in

  let rec visit node =
    if Hashtbl.mem marked_nodes node then ()
    else
      let edges = Hashtbl.find_opt graph node in
      match edges with
      | Some edges ->
          edges |> List.iter (fun edge -> visit edge);
          Hashtbl.add marked_nodes node ();
          sorted_nodes := node :: !sorted_nodes
      | None -> ()
  in

  Hashtbl.to_seq_keys graph |> List.of_seq |> List.iter (fun node -> visit node);
  !sorted_nodes
in

let ways src dst =
  let ways_from = Hashtbl.create 600 in
  Hashtbl.add ways_from src 1;

  toposorted
  |> List.iter (fun node ->
      Hashtbl.find graph node
      |> List.iter (fun neighbor ->
          Hashtbl.replace ways_from neighbor
            (match Hashtbl.find_opt ways_from node with
            | Some k ->
                k
                + (Hashtbl.find_opt ways_from neighbor
                  |> Option.value ~default:0)
            | None -> 0)));
  Hashtbl.find ways_from dst
in

Printf.printf "%d, %d" (ways "you" "out")
  (ways "svr" "fft" * ways "fft" "dac" * ways "dac" "out")
