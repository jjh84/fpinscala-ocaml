module State(S : sig type t end) = struct 

  type state = S.t 

  type 'a t = state -> ('a * state)

  let return v = 
    fun s -> (v, s)

  let map m f = 
    fun s -> 
      let n, s' = m s in 
      (f n, s')

  let map2 ma mb f = 
    fun s ->
      let n1, s1 = ma s in 
      let n2, s2 = mb s1 in 
      (f n1 n2, s2)

  let flatMap m f = 
    fun s -> 
      let n, s' = m s in 
      f n s'
end

module Int64StateMonad = State(struct type t = int64 end)

module SimpleRng = struct 
  
  type state = int64

  module M = State(struct type t = state end)

  let ( +! ) = Int64.add 
  let ( *! ) = Int64.mul
  let ( &! ) = Int64.logand

  let create state = 
    (0, state)

  let nextInt state =
    let newState = (state *! 0x5DEECE66DL +! 0xBL) &! 0xFFFFFFFFFFFFL in 
    let n = Int64.shift_right_logical newState 16 |> Int64.to_int in 
    (n, newState)

  let int = 
    nextInt 

  let randomPair state = 
    let n1, state1 = nextInt state in 
    let n2, state2 = nextInt state1 in 
    ((n1, n2), state2)

  let nonNegativeInt state = 
    let n1, state1 = nextInt state in 
    if n1 < 0 then 
      (abs(n1+1), state1)
    else 
      (n1, state1)

  let both ra rb = 
    M.map2 ra rb (fun a b -> (a, b))

  let randIntPair = 
    both nextInt nextInt

  let nonNegativeEven = 
    M.map nonNegativeInt (fun v -> v - (v mod 2))

  let nonNegativeLessThan n = 
    M.map nonNegativeInt (fun v -> v mod n)

  let rec nonNegativeLessThan_v2 n =
    fun rng ->
      let n1, state1 = nonNegativeInt rng in 
      let n2 = n1 mod n in 
      if (n1 + (n-1) - n2 >= 0) then 
        (n2, state1)
      else 
        nonNegativeLessThan_v2 n state1

  let rec nonNegativeLessThan_v3 n = 
    M.flatMap nonNegativeInt (fun v -> 
      let n = v mod n in 
      if (v + (n-1) - n >= 0) then M.return n 
      else nonNegativeLessThan_v3 n)

  let map r f = 
    M.flatMap r (fun v -> M.return (f v))

end

let () =
  let n, rng1 = SimpleRng.create 42L in
  let n1, rng2 = SimpleRng.int rng1 in
  Printf.printf "%d\n" n1;
  let n2, rng3 = SimpleRng.int rng2 in 
  Printf.printf "%d\n" n2;
  let n3, rng4 = SimpleRng.nonNegativeLessThan_v3 1000 rng3 in 
  Printf.printf "%d\n" n3
