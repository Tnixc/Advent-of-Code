let input = String.trim {|
#include "6"
|} in

input |> String.split_on_char '\n' |> List.rev |> function
| [] -> assert false
| last :: rest ->
    (List.rev rest, last) |> fun (operands, operators) ->
    let prev = ref 0 in
    let p1, p2 =
      [ String.length operators + 1 ]
      @ (String.to_seq operators |> List.of_seq
        |> List.fold_left
             (fun (acc, i) c ->
               if c = '*' || c = '+' then (i :: acc, i + 1) else (acc, i + 1))
             ([], 0)
        |> fst)
      |> List.rev |> List.tl
      |> List.map (fun i ->
          let opands =
            List.map (fun l -> String.sub l !prev (i - !prev)) operands
          in
          let r =
            String.sub operators !prev (min i (String.length operators) - !prev)
            |> String.trim
          in
          prev := i;
          (opands, r))
      |> List.mapi (fun idx (opands, r) ->
          let p2 =
            List.init (String.length (List.hd opands)) (fun i -> i)
            |> List.map (fun idx ->
                opands
                |> List.map (fun x ->
                    x |> String.to_seq |> List.of_seq
                    |> List.map (fun c -> String.make 1 c))
                |> List.fold_left
                     (fun acc digits ->
                       acc
                       ^ (List.nth_opt digits idx |> Option.value ~default:""))
                     "")
            |> List.map String.trim
            |> List.filter (fun s -> s <> "")
            |> List.map int_of_string
          in

          let p1 = opands |> List.map String.trim |> List.map int_of_string in
          match r with
          | "*" -> (List.fold_left ( * ) 1 p1, List.fold_left ( * ) 1 p2)
          | "+" -> (List.fold_left ( + ) 0 p1, List.fold_left ( + ) 0 p2)
          | _ -> assert false)
      |> List.fold_left
           (fun (acc1, acc2) (x1, x2) -> (acc1 + x1, acc2 + x2))
           (0, 0)
    in

    Printf.printf "%d, %d\n" p1 p2
