let pf = Format.printf

let gen_test size =
  pf "%d %d@." size 100_000;
  pf "%d" (Random.int 100_000);
  for i = 2 to size do
    pf " %d" (Random.int 100_000)
  done;
  pf "@.";
  pf "%d" (Random.int 100_000);
  for i = 2 to size do
    pf " %d" (Random.int 100_000)
  done;
  pf "@."

let () =
  pf "%d@." 100;
  for t = 1 to 92 do
    gen_test 1_000
  done;
  for t = 1 to 8 do
    gen_test 100_000
  done
