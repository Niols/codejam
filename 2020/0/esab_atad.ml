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

module Write = struct
  let int = string_of_int
  let string x = x

  let line1 t v =
    t v |> print_endline

  let line2g t1 v1 t2 v2 =
    t1 v1 ^ " " ^ t2 v2 |> print_endline

  let line3g t1 v1 t2 v2 t3 v3 =
    t1 v1 ^ " " ^ t2 v2 ^ " " ^ t3 v3 |> print_endline

  let line4g t1 v1 t2 v2 t3 v3 t4 v4 =
    t1 v1 ^ " " ^ t2 v2 ^ " " ^ t3 v3 ^ " " ^ t4 v4 |> print_endline

  let line5g t1 v1 t2 v2 t3 v3 t4 v4 t5 v5 =
    t1 v1 ^ " " ^ t2 v2 ^ " " ^ t3 v3 ^ " " ^ t4 v4 ^ " " ^ t5 v5 |> print_endline

  let line2 t v1 v2 = line2g t v1 t v2
  let line3 t v1 v2 v3 = line3g t v1 t v2 t v3
  let line4 t v1 v2 v3 v4 = line4g t v1 t v2 t v3 t v4
  let line5 t v1 v2 v3 v4 v5 = line5g t v1 t v2 t v3 t v4 t v5
end

(* ========================================================================== *)

let request turn p =
  Write.(line1 int (p + 1));
  incr turn;
  Read.(line1 bit)

let complement b =
  for j = 0 to Array.length b - 1 do
    b.(j) <- not b.(j)
  done

let reversed_indice b j =
  Array.length b - 1 - j

let reverse b =
  for j = 0 to Array.length b / 2 - 1 do
    let t = b.(j) in
    b.(j) <- b.(reversed_indice b j);
    b.(reversed_indice b j) <- t
  done

let detect_and_correct_complementation b turn read =
  (* Find a position where reversal does not change a thing. That way, by
     querying it, we can see if complementation took place. *)
  let j = ref 0 in
  while !j < !read / 2 do
    if b.(!j) = b.(reversed_indice b !j) then
      ( (* found it! *)
        if request turn !j = b.(!j) then
          ()
        else
          complement b;

        (* get off the loop *)
        j := max_int
      )
    else
      incr j
  done

let detect_and_correct_reversal b turn read =
  (* Find a position where reversal does change a thing. That way, by querying
     it, we can see if it has indeed been reversed. This function must be called
     after complementation. *)
  let j = ref 0 in
  while !j < !read / 2 do
    if b.(!j) <> b.(reversed_indice b !j) then
      ( (* found it! *)
        if request turn !j = b.(!j) then
          ()
        else
          reverse b;

        (* get off the loop *)
        j := max_int
      )
    else
      incr j
  done

let solve b =
  let lb = Array.length b in
  let turn = ref 0 in
  let read = ref 0 in
  b.(!read) <- request turn !read;
  incr read;
  while !read < lb do
    if !turn mod 10 = 0 then (* the array is going to be changed possibly *)
      (
        detect_and_correct_complementation b turn read;
        detect_and_correct_reversal b turn read;

        (* we need to always request bits 2 by 2 *)
        if !turn mod 2 = 1 then
          ignore (request turn 0);
      )
    else
      (
        let to_read = if !read mod 2 = 0 then !read / 2 else lb - !read / 2 - 1 in

        b.(to_read) <- request turn to_read;
        incr read
      )
  done;
  for i = 0 to lb - 1 do
    print_char (if b.(i) then '1' else '0')
  done;
  print_newline ()

let () =
  let (t, b) = Read.(line2 int) in
  let b = Array.make b false in
  for x = 1 to t do
    solve b;
    if Read.(line1 char) <> 'Y' then exit 0
  done
