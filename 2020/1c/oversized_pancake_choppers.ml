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

let minimum = ref max_int
let maximum = ref min_int
let slices = Hashtbl.create 300
let approx_slices = Hashtbl.create 300
let minimum_cuts = ref max_int

let init () =
  minimum := max_int;
  maximum := min_int;
  Hashtbl.reset slices;
  Hashtbl.reset approx_slices;
  minimum_cuts := max_int;
  ()

let iof = int_of_float
let foi = float_of_int

let feed d a =
  for i = 1 to d do
    let da = a /. (foi i) in
    let slices_da =
      try
        Hashtbl.find slices da
      with
        Not_found ->
        let slices_da = Array.make d 0 in
        Hashtbl.add slices da slices_da;
        slices_da
    in
    slices_da.(i - 1) <- slices_da.(i - 1) + 1
  done

let feed_approx las =
  List.iter
    (fun a ->
       Hashtbl.iter
         (fun a' slices_a' ->
            if a > a' then
              let n = a /. a' in
              if a' *. n = a then
                ()
              else
                let approx_slices_a' =
                  try Hashtbl.find approx_slices a'
                  with Not_found -> 0
                in
                Hashtbl.replace approx_slices a' (approx_slices_a' + (iof n))
         )
         slices
    )
    las

let feed_all d las =
  List.iter (feed d) las;
  feed_approx las

let rec compute_cuts_for_one a d i c = function
  | [] ->
    let approx_slices_a = try Hashtbl.find approx_slices a with Not_found -> 0 in
    if d < approx_slices_a then
      c + d
    else
      max_int
  | slices_i :: others ->
    let k = iof (0.5 +. ceil ((foi d) /. (foi (i+1)))) in
    if k <= slices_i then
      let d = d - (i+1) * (k-1) in
      let c = c + i * (k-1) in
      if d = i + 1 then
        c + i
      else
        compute_cuts_for_one a d (i+1) c []
    else
      let d = d - (i+1) * slices_i in
      let c = c + i * slices_i in
      compute_cuts_for_one a d (i+1) c others

let compute_cuts d =
  Hashtbl.iter
    (fun a slices_a ->
       let cuts = compute_cuts_for_one a d 0 0 (Array.to_list slices_a) in
       if cuts < !minimum_cuts then minimum_cuts := cuts
    )
    slices

let solve _n d las =
  let las = List.map float_of_int las in
  init ();
  feed_all d las;
  compute_cuts d;
  Format.printf "%d" !minimum_cuts

let () =
  let t = Read.(line1 int) in
  for i = 1 to t do
    let (n, d) = Read.(line2 int) in
    Format.printf "Case #%d: " i;
    solve n d Read.(line int);
    Format.printf "@."
  done
