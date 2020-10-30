open Cmdliner

let cmds = [Wc.cmd ; CandyMachine.cmd ]

let run () =
  let message = {|
Functional Programming in Scala in OCaml

Usage:
    fpiso COMMAND

Available Commands:
    candyMachine
    wc
  |}
  in
  Caml.print_endline message;
  0

let doc = "FP in Scala in OCaml"

let sdocs = Manpage.s_common_options

let man =
  [`S Manpage.s_description
  ; `P "Use $(mname) $(i,COMMAND) --help for help on a single command."
  ; `S Manpage.s_commands
  ]

let default_cmd =
  let term =
    let open Common.Let_syntax in
    let+ _term = Common.term in
    run ()
  in
  ( term
  , Term.info "fpiso" ~version:"%%VERSION%%" ~doc ~sdocs ~man )

let () = 
  Term.(exit_status @@ eval_choice default_cmd cmds)
