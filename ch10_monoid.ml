
module type Monoid = sig
  type t
  val op : t -> t -> t
  val zero : t
end

module StringMonoid : Monoid with type t = string = struct
  type t = string
  let op = (^)
  let zero = ""
end

module type TYPE = sig type t end;;

module ListMonoid (T : TYPE) : Monoid with type t = T.t list = struct
    type t = T.t list
    let op xs ys = xs @ ys
    let zero = []
end

module IntAddition : Monoid with type t = int = struct
  type t = int
  let op = (+)
  let zero = 0
end

module IntMultiplication : Monoid with type t = int = struct
  type t = int
  let op = ( * )
  let zero = 1
end

module BooleanOr : Monoid with type t = bool = struct
  type t = bool

  let op = (||)
  let zero = false
end

module BooleanAnd : Monoid with type t = bool = struct
  type t = bool

  let op = (&&)
  let zero = true
end

module Concatenate (M : Monoid) = struct 
  let concatenate lst = List.fold_left M.op M.zero lst
end

module SC = Concatenate (StringMonoid)
module IC = Concatenate (IntAddition)

let () = 
  let lst = ["Hic"; "Est"; "Index"] in 
  let l = List.fold_left StringMonoid.op StringMonoid.zero lst in
  let r = List.fold_right StringMonoid.op lst StringMonoid.zero in
  let c = SC.concatenate lst in
  let s = IC.concatenate [1;2;3;4;5;6] in
  Printf.printf "%s %s %s %d\n" l r c s
