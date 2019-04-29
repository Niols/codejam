let (||>) f g x = f x |> g

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

module Readline = struct
  let int = int_of_string
  let char s = assert (String.length s = 1); s.[0]
  let float = float_of_string
  let string = fun s -> s

  let list ?(sep=' ') cast =
    String.split_on_char sep ||> List.map cast
  let array ?sep cast =
    list ?sep cast ||> Array.of_list

  let pair ?sep ca cb s =
    match list ?sep string s with
    | [a; b] -> (ca a, cb b)
    | _ -> failwith "Readline.pair"

  let triple ?sep ca cb cc s =
    match list ?sep string s with
    | [a; b; c] -> (ca a, cb b, cc c)
    | _ -> failwith "Readline.triple"

  let quadruple ?sep ca cb cc cd s =
    match list ?sep string s with
    | [a; b; c; d] -> (ca a, cb b, cc c, cd d)
    | _ -> failwith "Readline.quadruple"

  let read cast = read_line () |> cast
end

(* ========================================================================== *)

let select7 = 0b1111111

let pow2 n = 1 lsl n

let solve_test () =
  print_endline "200";
  let n200 = input_line stdin |> int_of_string in
  (* n200 = R1.2^... + R2.2^... + R3.2^66 + R4.2^50 + R5.2^40 + R6.2^33 *)

  let r4 = (n200 lsr 50) land select7 in
  let r5 = (n200 lsr 40) land select7 in
  let r6 = (n200 lsr 33) land select7 in

  print_endline "54";
  let n54 = input_line stdin |> int_of_string in
  (* n54 = R1.2^54 + R2.2^27 + R3.2^18 + R4.2^13 + R5.2^10 + R6.2^9 *)
  let n54 = n54 - (r4 * pow2 13) - (r5 * pow2 10) - (r6 * pow2 9) in

  let r1 = (n54 lsr 54) land select7 in
  let r2 = (n54 lsr 27) land select7 in
  let r3 = (n54 lsr 18) land select7 in

  Format.printf "%d %d %d %d %d %d@." r1 r2 r3 r4 r5 r6

let solve () =
  let (t, _w) = Readline.(read (pair int string)) in
  for t_i = 1 to t do
    solve_test ();
    if Readline.(read string) = "-1" then
      exit 1
  done

let () = solve ()
