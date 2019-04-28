(* Code Jam - Qualif Round - You Can Go Your Own Way *)

type problem = (char list) list

type solution = (char list) list

let parse_problem () =
  let nb_cases = int_of_string (input_line stdin) in
  let res = ref [] in
  for i = 1 to nb_cases do
    let dim = int_of_string (input_line stdin) in
    let route =
      let route = ref [] in
      input_line stdin
      |> String.iter
        (fun c ->
           route := c :: !route);
      List.rev !route
    in
    assert (List.length route = 2 * dim - 2);
    res := route :: !res
  done;
  List.rev !res

let dim_of_list l =
  (List.length l) / 2 + 1

let print_solution solution =
  List.iteri
    (fun i route ->
       print_string "Case #";
       print_int (i+1);
       print_string ": ";
       List.iter print_char route;
       print_newline ())
    solution

let solve_one =
  List.map
    (function
      | 'E' -> 'S'
      | 'S' -> 'E'
      | _ -> assert false)

let solve_all = List.map solve_one

let () =
  parse_problem ()
  |> solve_all
  |> print_solution
