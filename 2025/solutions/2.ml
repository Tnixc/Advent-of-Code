let input = String.trim {|
#include "2"
|} in

let rec check2 str len span span_len =
  if len = 0 then true
  else if len < span_len || len mod span_len <> 0 then false
  else if String.sub str 0 span_len <> span then false
  else
    check2
      (String.sub str span_len (len - span_len))
      (len - span_len) span span_len
in

let p1, p2 =
  String.split_on_char ',' input
  |> List.map (fun x ->
      match String.split_on_char '-' x with
      | [ a; b ] -> (int_of_string a, int_of_string b)
      | _ -> assert false)
  |> List.map (fun (a, b) ->
      let rec range i (acc1, acc2) =
        if i > b then (acc1, acc2)
        else
          let str = string_of_int i in
          let len = String.length str in
          if
            len mod 2 = 0
            && String.sub str 0 (len / 2) = String.sub str (len / 2) (len / 2)
          then range (i + 1) (acc1 + i, acc2 + i)
          else
            let rec loop k count =
              if k > len / 2 then count
              else if check2 str len (String.sub str 0 k) k then count + i
              else loop (k + 1) count
            in
            range (i + 1) (acc1, acc2 + loop 1 0)
      in

      range a (0, 0))
  |> List.fold_left (fun (l1, l2) (r1, r2) -> (l1 + r1, l2 + r2)) (0, 0)
in

Printf.printf "%d, %d\n" p1 p2
