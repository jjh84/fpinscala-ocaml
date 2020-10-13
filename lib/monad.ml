module type MonadIntf = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type StateMonadIntf = sig
    include MonadIntf
    type state
    val put : state -> unit t
    val get : state t
    val runState: 'a t -> init:state -> 'a * state
end

module OptionMonad : MonadIntf with type 'a t = 'a option = struct
    type 'a t = 'a option

    let return x = Some x

    let bind m f =
        match m with 
        | Some a -> f a
        | None -> None
end

module WriterMonad : MonadIntf with type 'a t = 'a * string = struct
    type 'a t = 'a * string

    let return x = (x, "")

    let bind m f =
        let (x, s1) = m in
        let (y, s2) = f x in
        (y, s1 ^ s2)
end

module StateMonad(S : sig type t end) : StateMonadIntf with type state := S.t = struct
    type state = S.t

    type 'a t = state -> ('a * state)

    (* 함수 형태의 문맥에 감싸져 있는 값을 얻어와 일반함수에 적용 후 다시 함수 문맥에 넣어서 반환 *)
    let bind (m:state->('a*state)) f =
        fun s ->
            let (x, s') = m s in  (* state 전파 *)
            (f x) s'

    (* Insert a value into context *)
    let return a = fun s -> (a, s)

    let runState m ~init = m init

    let put s = fun _ -> ((), s)

    let get = fun s -> (s, s)
end

module MonadSyntax (M:MonadIntf) = struct
    include M

    (* New bind syntax since OCaml 4.08.0 *)
    let ( let* ) = M.bind

    let ( >>= ) = M.bind

    let ( >>| ) m f =
        m >>= (fun a -> return (f a))
end

module IntStateMonad = StateMonad(struct type t = int end)
module IntStateMonadSyntax = MonadSyntax(IntStateMonad)
module WriterMonadSyntax = MonadSyntax(WriterMonad)

let stateMonadExample ~state =
    let open IntStateMonad in
    let open IntStateMonadSyntax in
    let increase_state =
        let* i = get in
        put (i+1)
    in
    let get_int =
        let* i = get in
        let* () = increase_state in
        return i
    in
    runState get_int ~init:state

let writerMonadExample x =
    let open WriterMonadSyntax in
    let inc x = (x+1, Printf.sprintf "called with %d\n" x) in
    let dec x = (x-1, Printf.sprintf "called with %d\n" x) in
    (* I think >>= operator is more readable in this computation *)
    return x >>= inc >>= inc >>= dec
