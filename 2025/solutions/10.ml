let input = String.trim {|
#include "10"
|}

type machine = {
  lights : bool array;
  buttons : int list array;
  reqs : int array;
}

let parse_input =
  input |> String.split_on_char '\n'
  |> List.map (fun line ->
      let parts = String.split_on_char ']' line in
      let lights_str =
        String.sub (List.hd parts) 1 (String.length (List.hd parts) - 1)
      in
      let lights =
        Array.init (String.length lights_str) (fun i -> lights_str.[i] = '#')
      in

      let rest = List.nth parts 1 in
      let parts2 = String.split_on_char '{' rest in
      let buttons_str = List.hd parts2 in
      let reqs_str =
        String.sub (List.nth parts2 1) 0 (String.length (List.nth parts2 1) - 1)
      in

      let buttons =
        let parts = String.split_on_char ')' buttons_str in
        List.filter (fun s -> String.contains s '(') parts
        |> List.map (fun s ->
            let content = List.nth (String.split_on_char '(' s) 1 in
            String.split_on_char ',' content
            |> List.map (fun x -> int_of_string (String.trim x)))
        |> Array.of_list
      in

      let reqs =
        String.split_on_char ',' reqs_str
        |> List.map (fun x -> int_of_string (String.trim x))
        |> Array.of_list
      in

      { lights; buttons; reqs })

let solve_part1 machines =
  let rec combinations k lst =
    if k = 0 then [ [] ]
    else
      match lst with
      | [] -> []
      | x :: xs ->
          let with_x =
            List.map (fun rest -> x :: rest) (combinations (k - 1) xs)
          in
          let without_x = combinations k xs in
          with_x @ without_x
  in

  machines
  |> List.map (fun m ->
      let lights = m.lights in
      let buttons = m.buttons |> Array.to_list in
      let rec try_n_buttons n =
        if n > List.length buttons then max_int
        else
          let initial = Array.copy lights in
          let working = Array.copy initial in
          let combs = combinations n buttons in
          let results =
            combs
            |> List.filter_map (fun pressed ->
                Array.blit initial 0 working 0 (Array.length initial);
                pressed
                |> List.iter (fun button_lights ->
                    button_lights
                    |> List.iter (fun light_i ->
                        working.(light_i) <- not working.(light_i)));
                if Array.for_all not working then Some (List.length pressed)
                else None)
          in
          match results with [] -> try_n_buttons (n + 1) | xs -> List.hd xs
      in
      try_n_buttons 1)
  |> List.fold_left ( + ) 0

module Q = struct
  type t = { num : int; den : int }

  let gcd a b =
    let rec aux a b = if b = 0 then a else aux b (a mod b) in
    aux (abs a) (abs b)

  let make n d =
    if d = 0 then failwith "Division by zero";
    let g = gcd n d in
    let s = if d < 0 then -1 else 1 in
    { num = s * (n / g); den = s * (d / g) }

  let add a b = make ((a.num * b.den) + (b.num * a.den)) (a.den * b.den)
  let sub a b = make ((a.num * b.den) - (b.num * a.den)) (a.den * b.den)
  let mul a b = make (a.num * b.num) (a.den * b.den)
  let div a b = make (a.num * b.den) (a.den * b.num)
  let of_int n = { num = n; den = 1 }
  let zero = { num = 0; den = 1 }
  let one = { num = 1; den = 1 }
  let is_zero a = a.num = 0
  let is_int a = a.den = 1
  let floor a = a.num / a.den
  let ceil a = (a.num + a.den - 1) / a.den
end

let solve_part2 machines =
  let solve_machine m =
    let num_reqs = Array.length m.reqs in
    let num_buttons = Array.length m.buttons in

    (* matrix A (num_reqs x num_buttons) and vector b (num_reqs) *)
    let mat = Array.make_matrix num_reqs num_buttons Q.zero in
    for j = 0 to num_buttons - 1 do
      List.iter (fun i -> mat.(i).(j) <- Q.one) m.buttons.(j)
    done;

    let b = Array.map Q.of_int m.reqs in

    (* Gaussian elimination to row reduced echelon form *)
    let aug = Array.make_matrix num_reqs (num_buttons + 1) Q.zero in
    for i = 0 to num_reqs - 1 do
      for j = 0 to num_buttons - 1 do
        aug.(i).(j) <- mat.(i).(j)
      done;
      aug.(i).(num_buttons) <- b.(i)
    done;

    let pivot_row = ref 0 in
    let pivot_cols = Array.make num_reqs (-1) in
    (* row -> col *)
    let is_pivot_col = Array.make num_buttons false in

    for j = 0 to num_buttons - 1 do
      if !pivot_row < num_reqs then (
        let pivot = ref (-1) in
        for i = !pivot_row to num_reqs - 1 do
          if not (Q.is_zero aug.(i).(j)) then pivot := i
        done;

        if !pivot <> -1 then (
          (* swap rows *)
          let temp = aug.(!pivot_row) in
          aug.(!pivot_row) <- aug.(!pivot);
          aug.(!pivot) <- temp;

          (* normalize pivot row *)
          let pivot_val = aug.(!pivot_row).(j) in
          for k = j to num_buttons do
            aug.(!pivot_row).(k) <- Q.div aug.(!pivot_row).(k) pivot_val
          done;

          (* eliminate other rows *)
          for i = 0 to num_reqs - 1 do
            if i <> !pivot_row then
              let factor = aug.(i).(j) in
              if not (Q.is_zero factor) then
                for k = j to num_buttons do
                  aug.(i).(k) <-
                    Q.sub aug.(i).(k) (Q.mul factor aug.(!pivot_row).(k))
                done
          done;

          pivot_cols.(!pivot_row) <- j;
          is_pivot_col.(j) <- true;
          incr pivot_row))
    done;

    let rank = !pivot_row in

    (* check consistency for zero rows *)
    let consistent = ref true in
    for i = rank to num_reqs - 1 do
      if not (Q.is_zero aug.(i).(num_buttons)) then consistent := false
    done;

    if not !consistent then 0
    else
      (* free variables *)
      let free_cols = ref [] in
      for j = 0 to num_buttons - 1 do
        if not is_pivot_col.(j) then free_cols := j :: !free_cols
      done;
      let free_cols = Array.of_list (List.rev !free_cols) in
      let num_free = Array.length free_cols in

      (* precompute bounds for all variables based on original constraints *)
      (* x_j <= min(b_i / A_ij) for A_ij > 0 *)
      let max_vals = Array.make num_buttons max_int in
      for j = 0 to num_buttons - 1 do
        for i = 0 to num_reqs - 1 do
          if not (Q.is_zero mat.(i).(j)) then
            let limit = m.reqs.(i) in
            (* A_ij is 1 *)
            if limit < max_vals.(j) then max_vals.(j) <- limit
        done
      done;

      (* x_{pivot_i} = aug[i][last] - sum_{k in free} aug[i][k] * x_k *)
      let min_total = ref (-1) in

      let rec search free_idx current_free_vals =
        if free_idx = num_free then (
          (* pivots *)
          let valid = ref true in
          let current_sum = ref 0 in

          (* free vars to sum *)
          for k = 0 to num_free - 1 do
            current_sum := !current_sum + current_free_vals.(k)
          done;

          for i = 0 to rank - 1 do
            if !valid then (
              let mutable_val = ref aug.(i).(num_buttons) in
              for k = 0 to num_free - 1 do
                let f_col = free_cols.(k) in
                let coeff = aug.(i).(f_col) in
                if not (Q.is_zero coeff) then
                  mutable_val :=
                    Q.sub !mutable_val
                      (Q.mul coeff (Q.of_int current_free_vals.(k)))
              done;

              let v = !mutable_val in
              if Q.is_int v && v.num >= 0 then
                current_sum := !current_sum + v.num
              else valid := false)
          done;

          if !valid then
            if !min_total = -1 || !current_sum < !min_total then
              min_total := !current_sum)
        else
          let f_col = free_cols.(free_idx) in
          let limit = max_vals.(f_col) in

          for v = 0 to limit do
            current_free_vals.(free_idx) <- v;

            let partial_sum = ref 0 in
            for k = 0 to free_idx do
              partial_sum := !partial_sum + current_free_vals.(k)
            done;

            if !min_total = -1 || !partial_sum < !min_total then
              search (free_idx + 1) current_free_vals
          done
      in

      search 0 (Array.make num_free 0);

      if !min_total = -1 then 0 else !min_total
  in
  List.fold_left (fun acc m -> acc + solve_machine m) 0 machines

let () =
  let machines = parse_input in
  let p1 = solve_part1 machines in
  let p2 = solve_part2 machines in
  Printf.printf "%d, %d\n" p1 p2
