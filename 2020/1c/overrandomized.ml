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

let rec pow x n =
  if n = 0 then 1
  else x * pow x (n-1)

let letters = Hashtbl.create 8

let order_first_letters l =
  List.iter
    (fun (_q, s) ->
       try
         let i = Hashtbl.find letters s.[0] in
         Hashtbl.replace letters s.[0] (i+1)
       with
       | Invalid_argument _ -> ()
       | Not_found -> Hashtbl.add letters s.[0] 1
    )
    l;
  let letters_l = ref [] in
  Hashtbl.iter (fun l o ->
      letters_l := (o, l) :: !letters_l)
    letters;
  List.sort compare !letters_l

let order_letters l =
  List.iter
    (fun (_q, s) ->
       String.iter (fun c ->
           try
             let i = Hashtbl.find letters c in
             Hashtbl.replace letters c (i+1)
           with
           | Invalid_argument _ -> ()
           | Not_found -> Hashtbl.add letters c 1)
         s)
    l;
  let letters_l = ref [] in
  Hashtbl.iter (fun l o ->
      letters_l := (o, l) :: !letters_l)
    letters;
  List.sort compare !letters_l

exception Found of char

let find_zero l =
  try
    List.iter
      (fun (_q, s) ->
         String.iter
           (fun c ->
              if not (Hashtbl.mem letters c) then
                raise (Found c)
           )
           s
      )
      l;
    assert false
  with
    Found c -> c

let solve u l =
  Hashtbl.reset letters;
  let order = order_letters l in
  assert (List.length order <= 10);
  match order with
  | [] -> assert false
  | (_o, l) :: others ->
    (* zero appears less than others *)
    Format.printf "%c" l;
    List.iter (fun (_o, l) ->
        Format.printf "%c" l)
      (List.rev others)

let solve_first_letter u l =
  Hashtbl.reset letters;
  let order = order_first_letters l in
  let order =
    if List.length order = 10 then
      (
        match order with
        | [] -> assert false
        | zero :: others -> others @ [zero]
      )
    else if List.length order = 9 then
      order @ [(0, find_zero l)]
    else
      assert false
  in
  List.iter (fun (_o, l) ->
      Format.printf "%c" l)
    (List.rev order)

let () =
  let t = Read.(line1 int) in
  for i = 1 to t do
    let u = Read.(line1 int) in
    let l = List.init 10_000 (fun _ ->
        let (q, r) = Read.(line2g (int, string)) in
        (q, String.uppercase r)
      )
    in
    Format.printf "Case #%d: " i;
    solve_first_letter u l;
    Format.printf "@."
  done
