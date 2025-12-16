let input = String.trim {|
#include "12"
|} in

let count_char s c =
  String.fold_left (fun acc ch -> if ch = c then acc + 1 else acc) 0 s
in

let shapes, regions =
  match String.split_on_char 'X' input |> List.map String.trim with
  | [ s; r ] ->
      ( (match String.split_on_char ':' s with
        | [ _; s1; s2; s3; s4; s5; s6 ] ->
            [ s1; s2; s3; s4; s5; s6 ] |> List.map String.trim
            |> List.map (String.split_on_char '\n')
            |> List.map (fun l -> [ List.nth l 0; List.nth l 1; List.nth l 2 ])
            |> List.map (String.concat "\n")
            |> List.map (fun s -> (s, count_char s '#'))
        | _ -> assert false),
        r |> String.split_on_char '\n'
        |> List.map (String.split_on_char ':')
        |> List.map (function
          | [ dim; amnts ] ->
              ( ( String.split_on_char 'x' dim |> List.map int_of_string
                |> function
                  | [ w; h ] -> (w, h)
                  | _ -> assert false ),
                amnts |> String.trim |> String.split_on_char ' '
                |> List.map int_of_string )
          | _ -> assert false) )
  | _ -> assert false
in

let p1 =
  List.map
    (fun ((w, h), amnts) ->
      let area = w * h in
      let total = ref 0 in
      List.iteri
        (fun i amnt ->
          let _, a = List.nth shapes i in
          total := !total + (a * amnt))
        amnts;
      if !total >= area then 0 else 1)
    regions
  |> List.fold_left ( + ) 0
in

Printf.printf "%d\n" p1
