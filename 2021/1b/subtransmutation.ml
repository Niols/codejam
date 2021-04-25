let (||>) f g x = f x |> g

let epf = Format.eprintf
let spf = Format.sprintf

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

  let split_on_char_n n char s =
    let rec split_on_char_n acc n ~from_ =
      if n <= 0 then List.rev (sub s from_ (length s - from_) :: acc)
      else
        match index_from_opt s from_ char with
        | None -> failwith "ExtString.split_on_char_n"
        | Some i -> split_on_char_n (sub s from_ (i - from_) :: acc) (n - 1) ~from_:(i + 1)
    in
    if n <= 0 then invalid_arg "ExtString.split_on_char_n"
    else split_on_char_n [] (n - 1) ~from_:0
end

module Read = struct
  type 'a cast = string -> 'a

  let int s =
    try int_of_string s with _ -> failwith "ExtRead.int"

  let bit s = int s = 1

  let float s =
    try float_of_string s with _ -> failwith "ExtRead.float"

  let char x =
    if String.length x <> 1 then
      failwith "ExtRead.char";
    x.[0]

  let string s = s

  let no_space_string s =
    if String.index_opt s ' ' <> None then
      failwith "ExtRead.no_space_string";
    s

  let list c s =
    if s = "" then []
    else String.split_on_char ' ' s |> List.map c

  let non_empty_list c s =
    match list c s with
    | [] -> failwith "ExtRead.non_empty_list"
    | l -> l

  let array c s = list c s |> Array.of_list
  let non_empty_array c s = non_empty_list c s |> Array.of_list

  let tuple2g c1 c2 s =
    match String.split_on_char_n 2 ' ' s with
    | [v1; v2] -> (c1 v1, c2 v2)
    | _ -> assert false

  let tuple2 c s = tuple2g c c s
  let pairg = tuple2g
  let pair = tuple2

  let tuple3g c1 c2 c3 s =
    match String.split_on_char_n 3 ' ' s with
    | [v1; v2; v3] -> (c1 v1, c2 v2, c3 v3)
    | _ -> assert false

  let tuple3 c s = tuple3g c c c s
  let tripleg = tuple3g
  let triple = tuple3

  let tuple4g c1 c2 c3 c4 s =
    match String.split_on_char_n 4 ' ' s with
    | [v1; v2; v3; v4] -> (c1 v1, c2 v2, c3 v3, c4 v4)
    | _ -> assert false

  let tuple4 c s = tuple4g c c c c s

  let tuple5g c1 c2 c3 c4 c5 s =
    match String.split_on_char_n 5 ' ' s with
    | [v1; v2; v3; v4; v5] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5)
    | _ -> assert false

  let tuple5 c s = tuple5g c c c c c s

  let of_string cast s = cast s

  let line_of_chan ichan cast = input_line ichan |> string cast
  let line cast = line_of_chan stdin cast
end

module Write = struct
  type 'a cast = 'a -> string

  let int = string_of_int
  let bit = function true -> "1" | _ -> "0"
  let float = Format.sprintf "%g"
  let char = String.make 1
  let string = fun x -> x

  let list c l = List.map c l |> String.concat " "

  let array c s = list c (Array.to_list s)

  let tuple2g c1 c2 (v1, v2) =
    c1 v1 ^ " " ^ c2 v2

  let tuple2 c t = tuple2g c c t
  let pairg = tuple2g
  let pair = tuple2

  let tuple3g c1 c2 c3 (v1, v2, v3) =
    c1 v1
    ^ " " ^ c2 v2
    ^ " " ^ c3 v3

  let tuple3 c t = tuple3g c c c t

  let tuple4g c1 c2 c3 c4 (v1, v2, v3, v4) =
    c1 v1
    ^ " " ^ c2 v2
    ^ " " ^ c3 v3
    ^ " " ^ c4 v4

  let tuple4 c t = tuple4g c c c c t

  let tuple5g c1 c2 c3 c4 c5 (v1, v2, v3, v4, v5) =
    c1 v1
    ^ " " ^ c2 v2
    ^ " " ^ c3 v3
    ^ " " ^ c4 v4
    ^ " " ^ c5 v5

  let tuple5 c t = tuple5g c c c c c t

  let to_string cast v = cast v

  let line_to_chan ochan cast v =
    string cast v |> output_string ochan;
    output_char ochan '\n'

  let line cast value = line_to_chan stdout cast value
  let line_to_err cast value = line_to_chan stderr cast value
end

let iterate_cases solver cast =
  let t = Read.(line int) in
  for x = 1 to t do
    let y = solver x in
    Write.(line (pairg string cast)) (spf "Case #%d:" x, y)
  done

(* * *)

let split a b t i =
  assert (t.(i) > 0);
  if i - a >= 0 then t.(i - a) <- t.(i - a) + t.(i);
  if i - b >= 0 then t.(i - b) <- t.(i - b) + t.(i);
  t.(i) <- 0

let rec find_right_most a b t i =
  if i < 0 then None
  else if t.(i) > 0 then Some i
  else find_right_most a b t (i - 1)

let find_right_most a b t =
  find_right_most a b t (Array.length t - 1)

let simulate a b t =
  let rec simulate () =
    match find_right_most a b t with
    | Some i ->
      split a b t i;
      simulate ()
    | None ->
      Array.fold_left (fun all_pos n -> all_pos && n >= 0) true t
  in
  simulate ()

let rec try_them_all a b us k =
  let limit = 1000 * Array.length us in
  if k >= limit then
    None
  else
    let t = Array.make limit 0 in
    for i = 0 to Array.length us - 1 do
      t.(i) <- - us.(i)
    done;
    t.(k) <- t.(k) + 1;
    if simulate a b t then
      Some (k + 1)
    else
      try_them_all a b us (k+1)

let solve _ =
  let (n, a, b) = Read.(line (triple int)) in
  let us = Read.(line (array int)) in
  try_them_all a b us 0

let impossible_or c = function
  | None -> "IMPOSSIBLE"
  | Some x -> c x

let () = iterate_cases solve Write.(impossible_or int)
