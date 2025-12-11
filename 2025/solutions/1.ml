let input = String.trim {|
#include "1"
|} in

let emod a b =
  let r = a mod b in
  if r < 0 then r + abs b else r
in

let final, p1, p2 =
  String.split_on_char '\n' input
  |> List.map (fun line ->
      let line = String.trim line in
      (line.[0], String.sub line 1 (String.length line - 1) |> int_of_string))
  |> List.fold_left
       (fun (dial, acc1, acc2) (char, count) ->
         match char with
         | 'L' ->
             ( emod (dial - count) 100,
               (acc1 + if emod (dial - count) 100 = 0 then 1 else 0),
               acc2
               +
               if dial = 0 then count / 100
               else if count >= dial then ((count - dial) / 100) + 1
               else 0 )
         | 'R' ->
             ( emod (dial + count) 100,
               (acc1 + if emod (dial + count) 100 = 0 then 1 else 0),
               acc2 + ((dial + count) / 100) )
         | _ -> assert false)
       (50, 0, 0)
in

Printf.printf "%d, %d\n" p1 p2
