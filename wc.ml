open Base
open Monoid

type wc = 
  | Stub of string
  | Part of string * int * string

module WCMonoid : Monoid with type t = wc = struct
  type t = wc

  let zero = Stub ""

  let op a b = 
    match a, b with
    | Stub x, Stub y -> Stub (x ^ y)
    | Stub x, Part (l, w, r) -> Part(x ^ l, w, r)
    | Part (l, w, r), Stub x -> Part(l, w, r ^ x)
    | Part (l1, w1, r1), Part (l2, w2, r2) -> begin
      match r1 ^ l2 with
      | "" -> Part (l1, w1 + w2, r2)
      | _ -> Part (l1, w1 + w2 + 1, r2)
      end
end
  
let wc so =
  let s = Option.value so ~default:"" in
  let gen c =
    if Char.is_whitespace c then Part ("", 0, "")
    else Stub (Char.to_string c)
  in
  let unstub s = Int.min (String.length s) 1
  in
  let p = String.fold ~f:(fun a x ->
      let p = gen x in 
      WCMonoid.op a p
      ) ~init:WCMonoid.zero s
  in
  match p with
  | Stub s -> unstub s
  | Part (l, w, r) -> (unstub l) + w + (unstub r)

let () = 
  let n = wc (Some ("hello world!")) in 
  Stdio.printf "%d\n" n
