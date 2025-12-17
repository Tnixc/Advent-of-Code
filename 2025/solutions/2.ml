let input = String.trim {|
#include "2"
|} in

let twice n digits =
  if digits mod 2 = 1 then false
  else
    let half = digits / 2 in
    let first = n / int_of_float (10. ** float_of_int half) in
    let second = n mod int_of_float (10. ** float_of_int half) in
    first = second
in

let repeats n digits =
  let rec try_period p =
    if p > digits / 2 then false
    else if digits mod p <> 0 then try_period (p + 1)
    else
      let div = int_of_float (10. ** float_of_int (digits - p)) in
      let pattern = n / div in
      let rec check remaining len =
        if len = 0 then true
        else
          let chunk_divisor = int_of_float (10. ** float_of_int (len - p)) in
          let chunk = remaining / chunk_divisor in
          if chunk <> pattern then false
          else check (remaining mod chunk_divisor) (len - p)
      in
      if check n digits then true else try_period (p + 1)
  in
  try_period 1
in

let p1, p2 =
  String.split_on_char ',' input
  |> List.map (fun x ->
      match String.split_on_char '-' x with
      | [ a; b ] -> (int_of_string a, int_of_string b)
      | _ -> assert false)
  |> List.map (fun (a, b) ->
      let p1 = ref 0 in
      let p2 = ref 0 in
      for i = a to b do
        let digits = float_of_int i |> log10 |> ceil |> int_of_float in
        if twice i digits then (
          p1 := !p1 + i;
          p2 := !p2 + i)
        else if repeats i digits then p2 := !p2 + i
      done;
      (!p1, !p2))
  |> List.fold_left (fun (l1, l2) (r1, r2) -> (l1 + r1, l2 + r2)) (0, 0)
in

Printf.printf "%d, %d\n" p1 p2
