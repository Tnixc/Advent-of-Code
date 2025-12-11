type point = { x : int; y : int; z : int }

let input = String.trim {|
#include "8"
|}

let square x = x * x

module UnionFind = struct
  type t = {
    parent : int array;
    size : int array;
    mutable component_count : int;
  }

  let make n =
    {
      parent = Array.init n (fun i -> i);
      size = Array.make n 1;
      component_count = n;
    }

  let rec find self i =
    let p = Array.get self.parent i in
    if p = i then i
    else
      let root = find self p in
      Array.set self.parent i root;
      root

  let union self i j =
    let root1 = find self i in
    let root2 = find self j in
    if root1 <> root2 then (
      let size1 = Array.get self.size root1 in
      let size2 = Array.get self.size root2 in
      if size1 < size2 then (
        Array.set self.parent root1 root2;
        Array.set self.size root2 (size1 + size2))
      else (
        Array.set self.parent root2 root1;
        Array.set self.size root1 (size1 + size2));
      self.component_count <- self.component_count - 1;
      true)
    else false

  let get_component_sizes uf =
    let sizes = Hashtbl.create 100 in
    for i = 0 to Array.length uf.parent - 1 do
      let root = find uf i in
      let current = try Hashtbl.find sizes root with Not_found -> 0 in
      Hashtbl.replace sizes root (current + 1)
    done;
    Hashtbl.fold (fun _ size acc -> size :: acc) sizes []
end

let solve points =
  let rounds = 1000 in
  let n_points = List.length points in
  let components = UnionFind.make n_points in

  let points_arr = Array.of_list points in

  let pairs_list = ref [] in

  for i = 0 to n_points - 1 do
    let p1 = Array.get points_arr i in
    for j = i + 1 to n_points - 1 do
      let p2 = Array.get points_arr j in
      let d =
        square (p1.x - p2.x) + square (p1.y - p2.y) + square (p1.z - p2.z)
      in
      if d < 210_675_305 then pairs_list := (d, (i, j)) :: !pairs_list
    done
  done;

  let all_pairs = Array.of_list (List.sort compare !pairs_list) in

  let len = Array.length all_pairs in
  let idx = ref 0 in

  for _round = 0 to rounds - 1 do
    if !idx < len then
      let _, (i, j) = Array.get all_pairs !idx in
      let _ = UnionFind.union components i j in
      incr idx
  done;

  let part1 =
    UnionFind.get_component_sizes components
    |> List.sort (fun a b -> compare b a)
    |> List.take 3 |> List.fold_left ( * ) 1
  in

  let final_idx1 = ref 0 in
  let final_idx2 = ref 0 in
  while components.component_count > 1 && !idx < len do
    let _, (i, j) = Array.get all_pairs !idx in
    let merged = UnionFind.union components i j in
    if merged then (
      final_idx1 := i;
      final_idx2 := j);
    incr idx
  done;

  let final_p1 = Array.get points_arr !final_idx1 in
  let final_p2 = Array.get points_arr !final_idx2 in

  Printf.printf "%d, %d\n" part1 (final_p1.x * final_p2.x)

let _ =
  input |> String.split_on_char '\n'
  |> List.map (fun l ->
      match String.split_on_char ',' l |> List.map int_of_string with
      | [ x; y; z ] -> { x; y; z }
      | _ -> failwith "invalid point format")
  |> solve
