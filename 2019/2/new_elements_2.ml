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

open Readline

type res =
  | Impossible
  | Possible of int * int

let ab molecules =
  let rec m ci ji a b = function
    | [] -> (a, b)
    | (ci1, ji1) :: molecules ->
      if ci = ci1 && ji1 <= ji then
        failwith "lkj";
      if ji1 > ji then
        (
          let r = (float_of_int (ci - ci1)) /. (float_of_int (ji1 - ji)) in
          m ci1 ji1 (max a r) b molecules
        )
      else if ji1 < ji then
        (
          let r = (float_of_int (ci - ci1)) /. (float_of_int (ji1 - ji)) in
          m ci1 ji1 a (min b r) molecules
        )
      else
        (
          if ci1 <= ci then
            failwith "sdkfh";
          m ci1 ji1 a b molecules
        )
  in
  match molecules with
  | [] -> assert false
  | (ci, ji) :: molecules ->
    m ci ji 0. infinity molecules

let try_c a b =
  let j_from_c c =
    let c = float_of_int c in
    let ac = a *. c in
    let j = ceil ac in
    let j = if j = ac then j +. 1. else j in
    if j < b *. c then
      Some (int_of_float j)
    else
      None
  in
  let rec pow2_c i =
    if i > 61 then
      failwith "smkfj";
    let c = 1 lsl i in
    match j_from_c c with
    | Some j -> i
    | None -> pow2_c (i+1)
  in
  let rec dyc_c c1 c2 = (* c1 ne marche pas, c2 marche *)
    let c3 = (c1 + c2) / 2 in
    if c3 = c1 then
      c2
    else
      match j_from_c c3 with (* on regarde si c3 marche *)
      | Some _ -> dyc_c c1 c3
      | None   -> dyc_c c3 c2
  in
  let i = pow2_c 0 in
  if i = 0 then
    (1, unwrap (j_from_c 1))
  else
    let c = dyc_c (1 lsl (i-1)) (1 lsl i) in
    (c, unwrap (j_from_c c))

let solve molecules =
  try
    let (a, b) = ab molecules in
    let (c, j) = try_c a b in
    Possible (c, j)
  with
    Failure _ -> Impossible

let () =
  let t = read int in
  for t_i = 1 to t do
    let n = read int in
    match solve (List.init n (fun _ -> read (pair int int))) with
    | Impossible ->
      Format.printf "Case #%d: IMPOSSIBLE@." t_i
    | Possible (c, j) ->
      Format.printf "Case #%d: %d %d@." t_i c j
  done
