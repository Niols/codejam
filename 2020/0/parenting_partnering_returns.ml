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

module Read = struct
  let int = int_of_string
  let bit = int ||> ((=) 1)
  let float = float_of_string
  let char x = assert (String.length x = 1); x.[0]
  let string x = x

  let line t =
    read_line ()
    |> String.split_on_char ' '
    |> List.map t

  let line_a t =
    line t |> Array.of_list

  let line1 t =
    t (read_line ())

  let line2g (t1, t2) =
    match read_line () |> String.split_on_char ' ' with
    | [x1; x2] -> (t1 x1, t2 x2)
    | _ -> assert false

  let line3g (t1, t2, t3) =
    match read_line () |> String.split_on_char ' ' with
    | [x1; x2; x3] -> (t1 x1, t2 x2, t3 x3)
    | _ -> assert false

  let line4g (t1, t2, t3, t4) =
    match read_line () |> String.split_on_char ' ' with
    | [x1; x2; x3; x4] -> (t1 x1, t2 x2, t3 x3, t4 x4)
    | _ -> assert false

  let line5g (t1, t2, t3, t4, t5) =
    match read_line () |> String.split_on_char ' ' with
    | [x1; x2; x3; x4; x5] -> (t1 x1, t2 x2, t3 x3, t4 x4, t5 x5)
    | _ -> assert false

  let line2 t = line2g (t, t)
  let line3 t = line3g (t, t, t)
  let line4 t = line4g (t, t, t, t)
  let line5 t = line5g (t, t, t, t, t)
end

(* ========================================================================== *)

let solve l =
  let bc = ref 0 in (* last busy minute of Cameron *)
  let bj = ref 0 in (* last busy minute of Jamie *)
  try
    l
    |> List.sort (fun (_, s1, e1) (_, s2, e2) -> compare (s1, e1) (s2, e2))
    |> List.map (fun (i, s, e) ->
        if !bc <= s then
          (
            bc := e;
            (i, "C")
          )
        else if !bj <= s then
          (
            bj := e;
            (i, "J")
          )
        else
          failwith "IMPOSSIBLE"
      )
    |> List.sort compare
    |> List.map snd
    |> String.concat ""
  with
    Failure "IMPOSSIBLE" -> "IMPOSSIBLE"

let () =
  let t = Read.(line1 int) in
  for x = 1 to t do
    let n = Read.(line1 int) in
    let l = List.init n (fun i -> let (s, e) = Read.(line2 int) in (i, s, e)) in
    Format.printf "Case #%d: %s@." x (solve l)
  done
