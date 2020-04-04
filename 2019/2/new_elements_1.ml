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

let prod = List.fold_left ( * ) 1

let cmp_pair_fst_only (x1, y1) (x2, y2) =
  compare x1 x2

let cmp_pair_snd_only (x1, y1) (x2, y2) =
  compare y1 y2

let check_sorted cmp l =
  let rec check_sorted prev = function
    | [] -> true
    | h :: t ->
      if cmp prev h <= 0 then
        check_sorted h t
      else
        false
  in
  match l with
  | [] -> true
  | h :: t -> check_sorted h t

let cmp_lexico = compare
let cmp_antilexico (x1, y1) (x2, y2) =
  let c = compare y1 y2 in
  if c = 0 then
    compare x1 x2
  else
    c

let group eq l =
  let rec group curr_elt curr_group = function
    | [] -> [List.rev curr_group]
    | h :: t ->
      if eq h curr_elt then
        group curr_elt (h :: curr_group) t
      else
        (List.rev curr_group) :: group h [h] t
  in
  match l with
  | [] -> []
  | h :: t -> group h [h] t

let eq_from_cmp cmp a b =
  cmp a b = 0

let solve t molecules =
  let wrt_fst =
    molecules
    |> List.sort cmp_pair_fst_only
    |> group (eq_from_cmp cmp_pair_fst_only)
    |> List.map
      (fun grp ->
         grp
         |> List.sort_uniq cmp_pair_snd_only
         |> group (eq_from_cmp cmp_pair_snd_only)
         |> List.map List.length
         |> prod)
    |> prod
  in
  let wrt_snd =
    molecules
    |> List.sort cmp_pair_snd_only
    |> group (eq_from_cmp cmp_pair_snd_only)
    |> List.map
      (fun grp ->
         grp
         |> List.sort_uniq cmp_pair_fst_only
         |> group (eq_from_cmp cmp_pair_fst_only)
         |> List.map List.length
         |> prod)
    |> prod
  in
  let wrt_both =
    let l =
      molecules
      |> List.sort_uniq cmp_lexico
    in
    if check_sorted cmp_antilexico l then
      (
        (* Format.eprintf "sorted wrt antilexico@."; *)
        l
        |> group (=)
        |> List.map List.length (* FIXME *)
        |> prod
      )
    else
      0
  in
  (* Format.eprintf "wrt_fst = %d@.wrt_snd = %d@.wrt_both = %d@." wrt_fst wrt_snd wrt_both; *)
  wrt_fst + wrt_snd - wrt_both

(* let () =
  Format.eprintf "%b@."
    (check_sorted compare [1; 2]) *)

let () =
  let open Readline in
  let t = read int in
  for t_i = 1 to t do
    let n = read int in
    Format.printf "Case #%d: %d@."
      t_i
      (solve t_i (List.init n (fun _ -> read (pair int int))))
  done
