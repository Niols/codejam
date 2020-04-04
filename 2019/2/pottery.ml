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

let () = Random.init 9876543

(* Randomize vases. *)
let vases = Array.init 20 (fun i -> i)
let () = Array.sort (fun _ _ -> Random.int 3 - 1) vases
(* let () = Format.eprintf "Ready.@." *)
let vase i = 1 + vases.(i-1)

open Readline

let n = 1 (* nb de vases gagnants *)

let play () =
  (* On attaque 20 - n vases. *)
  let loosing_vase = ref 0 in
  for day = 1 to 99 - n do
    assert (day = read int);
    incr loosing_vase;
    if !loosing_vase > 20 - n then
      loosing_vase := 1;
    Format.printf "%d %d@." (vase !loosing_vase) 1
  done;
  (* On lit n vases. *)
  let winning_vases = Array.make (n+1) 0 in
  for i = 1 to n do
    let day = 99 - n + i in
    assert (day = read int);
    (* Format.eprintf "Reading vase %d@." (vase (20 - n + i)); *)
    Format.printf "%d %d@." (vase (20 - n + i)) 0;
    winning_vases.(i) <- List.hd (read (list int));
    (* Format.eprintf "  It had %d tokens.@." winning_vases.(i) *)
  done;
  (* On choisit le meilleur. *)
  winning_vases.(0) <- max_int;
  let best = ref 0 in
  for i = 1 to n do
    if winning_vases.(i) < winning_vases.(!best) then
      best := i
  done;
  assert (100 = read int);
  (* Format.eprintf "Choosing vase %d@." (vase (20 - n + !best)); *)
  Format.printf "%d %d@." (vase (20 - n + !best)) 100

let () =
  let t = read int in
  for t_i = 1 to t do
    play ()
  done
