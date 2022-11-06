
module SimpleRng = struct 

  let ( +! ) = Int64.add 
  let ( *! ) = Int64.mul
  let ( &! ) = Int64.logand

  type seed = int64
  type 'a rand = seed -> (int * seed)

  let create seed = 
    (0, seed)

  let nextInt seed =
    let newSeed = (seed *! 0x5DEECE66DL +! 0xBL) &! 0xFFFFFFFFFFFFL in 
    let n = Int64.shift_right_logical newSeed 16 |> Int64.to_int in 
    (n, newSeed)

  let int : int rand = 
    nextInt 

  let randomPair seed = 
    let n1, seed1 = nextInt seed in 
    let n2, seed2 = nextInt seed1 in 
    ((n1, n2), seed2)

  let nonNegativeInt seed = 
    let n1, seed1 = nextInt seed in 
    if n1 < 0 then 
      (abs(n1+1), seed1)
    else 
      (n1, seed1)

  let unit v : int rand = 
    fun seed -> (v, seed)

  let map r f = 
    fun seed -> 
      let n1, seed1 = r seed in 
      (f n1, seed1)

  let map2 ra rb f = 
    fun seed ->
      let n1, seed1 = ra seed in 
      let n2, seed2 = rb seed1 in 
      (f n1 n2, seed2)

  let both ra rb = 
    map2 ra rb (fun a b -> (a, b))

  let randIntPair = 
    both nextInt nextInt

  let nonNegativeEven = 
    map nonNegativeInt (fun v -> v - (v mod 2))

  let nonNegativeLessThan n = 
    map nonNegativeInt (fun v -> v mod n)

  let rec nonNegativeLessThan_v2 n =
    fun rng ->
      let n1, seed1 = nonNegativeInt rng in 
      let n2 = n1 mod n in 
      if (n1 + (n-1) - n2 >= 0) then 
        (n2, seed1)
      else 
        nonNegativeLessThan_v2 n seed1

  let flatMap r f = 
    fun seed -> 
      let n1, seed1 = r seed in 
      f n1 seed1

  let rec nonNegativeLessThan_v3 n = 
    flatMap nonNegativeInt (fun v -> 
      let n = v mod n in 
      if (v + (n-1) - n >= 0) then unit n 
      else nonNegativeLessThan_v3 n)
end

let () =
  let n, rng1 = SimpleRng.create 42L in
  let n1, rng2 = SimpleRng.int rng1 in
  Printf.printf "%d\n" n1;
  let n2, rng3 = SimpleRng.int rng2 in 
  Printf.printf "%d\n" n2;
  let n3, rng4 = SimpleRng.nonNegativeLessThan_v3 1000 rng3 in 
  Printf.printf "%d\n" n3
