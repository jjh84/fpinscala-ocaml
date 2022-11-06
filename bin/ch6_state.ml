
module SimpleRng = struct 

  let ( +! ) = Int64.add 
  let ( *! ) = Int64.mul
  let ( &! ) = Int64.logand

  (* type t = (int * int64) *)

  let create seed = 
    (0, seed)

  let nextInt seed =
    let newSeed = (seed *! 0x5DEECE66DL +! 0xBL) &! 0xFFFFFFFFL in 
    let n = Int64.shift_right_logical newSeed 16 |> Int64.to_int in 
    (n, newSeed)

end

let () =
  let n, rng1 = SimpleRng.create 42L in
  let n1, rng2 = SimpleRng.nextInt rng1 in
  Printf.printf "%d\n" n1;
  let n2, _rng3 = SimpleRng.nextInt rng2 in 
  Printf.printf "%d\n" n2
