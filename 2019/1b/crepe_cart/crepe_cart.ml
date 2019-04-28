let unwrap = function
  | None -> failwith "unwrap"
  | Some x -> x

module List = struct
  include List

  let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | a :: l -> rev_append l (a :: l2)

  let append_tr l1 l2 =
    rev_append (rev l1) l2

  let rec init_tailrec_aux acc i n f =
    if i >= n then acc
    else init_tailrec_aux (f i :: acc) (i+1) n f

  let rec init_aux i n f =
    if i >= n then []
    else
      let r = f i in
      r :: init_aux (i+1) n f

  let rev_init_threshold = 10_000
  (* match Sys.backend_type with
   * | Sys.Native | Sys.Bytecode -> 10_000
   * (\* We don't known the size of the stack, better be safe and assume it's small. *\)
   * | Sys.Other _ -> 50 *)

  let init len f =
    if len < 0 then invalid_arg "List.init" else
    if len > rev_init_threshold then rev (init_tailrec_aux [] 0 len f)
    else init_aux 0 len f

  let rec find_opt p = function
    | [] -> None
    | x :: l -> if p x then Some x else find_opt p l
end

module String = struct
  include String

  let rec index_rec_opt s lim i c =
    if i >= lim then None else
    if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c

  let index_opt s c = index_rec_opt s (length s) 0 c

  let index_from_opt s i c =
    let l = length s in
    if i < 0 || i > l then invalid_arg "String.index_from_opt / Bytes.index_from_opt" else
      index_rec_opt s l i c

  let rec rindex_rec_opt s i c =
    if i < 0 then None else
    if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c

  let rindex_opt s c = rindex_rec_opt s (length s - 1) c

  let rindex_from_opt s i c =
    if i < -1 || i >= length s then
      invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
    else
      rindex_rec_opt s i c

  let split_on_char sep s =
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
        r := sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    sub s 0 !j :: !r

  let split2_on_char sep s =
    match index_opt s sep with
    | None -> failwith "split2_on_char"
    | Some i -> (sub s 0 i, sub s (i+1) (length s - i - 1))

  let () = assert (split2_on_char '/' "ab/cd" = ("ab", "cd"))
end

(* ========================================================================== *)

type point = int * int
type dir = string

type test =
  { q : int ;
    p : (int * int * dir) list }

type problem = test list

let problem =
  let t = int_of_string (input_line stdin) in
  let tests = ref [] in
  for t_i = 1 to t do
    let [p; q] = input_line stdin |> String.split_on_char ' ' |> List.map int_of_string in
    let people = ref [] in
    for p_i = 1 to p do
      let [x; y; d] = input_line stdin |> String.split_on_char ' ' in
      people := (int_of_string x, int_of_string y, d) :: !people
    done;
    tests := { q ; p = List.rev !people } :: !tests
  done;
  List.rev !tests

let find_y t =
  let diff = Array.make (t.q+1) 0 in
  List.iter
    (fun (_x, y, d) ->
       if d = "N" && y < t.q then
         diff.(y+1) <- diff.(y+1) + 1
       else if d = "S" then
         diff.(y) <- diff.(y) - 1)
    t.p;
  let curr = ref 0 in
  let max = ref 0 in
  let max_i = ref 0 in
  for i = 0 to t.q do
    curr := !curr + diff.(i);
    if !curr > !max then (* strict so that we take the first *)
      (max := !curr;
       max_i := i)
  done;
  !max_i

let find_x t =
  let diff = Array.make (t.q+1) 0 in
  List.iter
    (fun (x, _y, d) ->
       if d = "E" && x < t.q then
         diff.(x+1) <- diff.(x+1) + 1
       else if d = "W" then
         diff.(x) <- diff.(x) - 1)
    t.p;
  let curr = ref 0 in
  let max = ref 0 in
  let max_i = ref 0 in
  for i = 0 to t.q do
    curr := !curr + diff.(i);
    if !curr > !max then (* strict so that we take the first *)
      (max := !curr;
       max_i := i)
  done;
  !max_i

let solve t =
  let x = find_x t in
  let y = find_y t in
  (x, y)

let () =
  List.iteri
    (fun i test ->
       let (x, y) = solve test in
       Format.printf "Case #%d: %d %d@." (i+1) x y)
    problem
