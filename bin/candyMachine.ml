open Core_kernel
open Fpis_ocaml.Monad

type action = 
  | Coin 
  | Turn

type output = 
  | Candy 
  | CoinBack 
  | Nothing

type state = int * int * int  (* candies, your coins, my coins *)

module MachineStateMonad = StateMonad(struct type t = state end)
module MachineStateMonadSyntax = MonadSyntax(MachineStateMonad)

module Machine = struct
  open MachineStateMonad
  open MachineStateMonadSyntax

  let insert_coin output =
    let* candies, ycoins, mcoins = get in
    let* () = put (candies, ycoins+1, mcoins) in
    return (Nothing :: output)

  let get_candy output =
    let* candies, ycoins, mcoins = get in
    let* () = put (candies-1, ycoins-1, mcoins+1) in
    return (Candy :: output)

  let step action output =
    let* candies, ycoins, _ = get in
    match candies, action with
    | 0, Coin -> return (CoinBack :: output)
    | _, Coin -> insert_coin output
    | _, Turn when ycoins>0 -> get_candy output
    | _ -> return (Nothing :: output)

  let iterm ~sub =
    List.fold ~init:(return []) ~f:(fun m act -> 
      m >>= fun output -> sub act output) 

  let simulateMachine inputs =
    let open Printf in
    let m = iterm ~sub:step inputs in
    let out, (candies, ycoins, mcoins) = runState m ~init:(10, 0, 0) in
    let () = List.iter ~f:(fun output ->
      match output with
      | Candy -> printf "Got Candy!\n" 
      | Nothing -> printf "Nothing\n" 
      | CoinBack -> printf "Get CoinBack\n") (List.rev out) in
    printf "State => your coins: %d, my coins: %d, candies: %d)" ycoins mcoins candies
end

let start () =
  Machine.simulateMachine [Coin; Turn; Coin; Turn; Coin; Turn; Coin; Turn; Coin; Turn]
