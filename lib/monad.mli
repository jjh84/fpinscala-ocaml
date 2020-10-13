module type MonadIntf =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
module type StateMonadIntf =
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    type state
    val put : state -> unit t
    val get : state t
    val runState : 'a t -> init:state -> 'a * state
  end
module OptionMonad :
  sig
    type 'a t = 'a option
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
module WriterMonad :
  sig
    type 'a t = 'a * string
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
  end
module StateMonad :
  functor (S : sig type t end) ->
    sig
      type 'a t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val put : S.t -> unit t
      val get : S.t t
      val runState : 'a t -> init:S.t -> 'a * S.t
    end
module MonadSyntax :
  functor (M : MonadIntf) ->
    sig
      type 'a t = 'a M.t
      val return : 'a -> 'a t
      val bind : 'a t -> ('a -> 'b t) -> 'b t
      val ( let* ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
      val ( >>= ) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
      val ( >>| ) : 'a M.t -> ('a -> 'b) -> 'b M.t
    end
module IntStateMonad :
  sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val put : int -> unit t
    val get : int t
    val runState : 'a t -> init:int -> 'a * int
  end
module IntStateMonadSyntax :
  sig
    type 'a t = 'a IntStateMonad.t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) :
      'a IntStateMonad.t -> ('a -> 'b IntStateMonad.t) -> 'b IntStateMonad.t
    val ( >>= ) :
      'a IntStateMonad.t -> ('a -> 'b IntStateMonad.t) -> 'b IntStateMonad.t
    val ( >>| ) : 'a IntStateMonad.t -> ('a -> 'b) -> 'b IntStateMonad.t
  end
module WriterMonadSyntax :
  sig
    type 'a t = 'a WriterMonad.t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( let* ) :
      'a WriterMonad.t -> ('a -> 'b WriterMonad.t) -> 'b WriterMonad.t
    val ( >>= ) :
      'a WriterMonad.t -> ('a -> 'b WriterMonad.t) -> 'b WriterMonad.t
    val ( >>| ) : 'a WriterMonad.t -> ('a -> 'b) -> 'b WriterMonad.t
  end
val stateMonadExample : state:int -> int * int
val writerMonadExample : int -> int WriterMonad.t