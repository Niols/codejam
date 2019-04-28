(* Code Jam - Qualification Round. Foregone Solution. *)

type problem = string list

type solution = (string * string) list

let parse_problem () =
  let nb_cases = int_of_string (input_line stdin) in
  let cases = ref [] in
  for i = 1 to nb_cases do
    cases := input_line stdin :: !cases
  done;
  List.rev !cases

let print_solution =
  List.iteri
    (fun i (a, b) ->
       Printf.printf "Case #%d: %s %s\n" (i+1) a b)

let split_in_two nb =
  let l = String.length nb in
  let a = Bytes.create l in
  let b = Bytes.create l in
  for i = 0 to l - 1 do
    match nb.[i] with
    | '4' ->
      Bytes.set a i '3';
      Bytes.set b i '1';
    | _ ->
      Bytes.set a i nb.[i];
      Bytes.set b i '0'
  done;
  Bytes.to_string a, Bytes.to_string b

let split_all_in_two =
  List.map split_in_two

let () =
  parse_problem ()
  |> split_all_in_two
  |> print_solution
