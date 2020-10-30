open Base
open Cmdliner
open Fpis_ocaml.Monoid

let doc = "Word Count"

let sdocs = Manpage.s_common_options

let man_xrefs = [`Main]

let man = [
  `S Manpage.s_description
  ; `P "$(wc) will count words in given string."
  ]

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

let info = Term.info "wc" ~doc ~sdocs ~man ~man_xrefs

let term =
  let open Common.Let_syntax in
  let+ _term = Common.term
  and+ str =
    let doc = "String for word counting" in
    Arg.(value & pos 0 (some string) None & info [] ~doc)
  in
  Stdio.printf "wc: %d\n" (wc str);
  Ok () |> Common.handle_errors
  
let cmd = term, info 