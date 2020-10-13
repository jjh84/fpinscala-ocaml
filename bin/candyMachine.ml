open Core_kernel
open Fpis_ocaml.Monad

type action = 
  | Coin 
  | Turn

type output = 
  | Candy 
  | Locked 
  | Nothing

type state = bool * int * int  (* locked, candies, coins *)

module MachineStateMonad = StateMonad(struct type t = state end)
module MachineStateMonadSyntax = MonadSyntax(MachineStateMonad)

module Machine = struct
  open MachineStateMonad
  open MachineStateMonadSyntax

  let insert_coin output =
    let* _, candies, coins = get in
    let* () = put (false, candies, coins+1) in
    return (Nothing :: output)

  let get_candy output =
    let* _, candies, coins = get in
    let* () = put (true, candies-1, coins) in
    return (Candy :: output)

  let step action output =
    let* locked, candies, _ = get in
    match locked, candies, action with
    | _, 0, _ -> return (Nothing :: output)
    | true,  _, Coin -> insert_coin output
    | false, _, Coin -> return (Nothing :: output)
    | false, _, Turn -> get_candy output
    | true,  _, Turn -> return (Locked :: output)

  let iterm ~sub =
    List.fold ~init:(return []) ~f:(fun m act -> 
      m >>= fun output -> sub act output) 

  let simulateMachine inputs =
    let open Printf in
    let m = iterm ~sub:step inputs in
    let out, (locked, candies, coins) = runState m ~init:(true, 5, 10) in
    let () = List.iter ~f:(fun output ->
      match output with
      | Candy -> printf "Got Candy!\n" 
      | Nothing -> printf "Nothing\n" 
      | Locked -> printf "Locked\n") (List.rev out) in
    printf "State => your locked? %s, coins: %d, candies: %d" (string_of_bool locked) coins candies
end

let start () =
  Machine.simulateMachine [Coin; Turn; Coin; Turn; Coin; Turn; Coin; Turn]
