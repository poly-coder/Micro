namespace Micro

[<RequireQualifiedAccess>]
module Option =
  (*
    Returns the option value or evaluates the given function if its value is None
  *)
  let byDefaultFn defaultValueFn = function
    | Some v -> v
    | None -> defaultValueFn()

  (*
    Returns the option value or the given default value if its value is None
  *)
  let byDefault defaultValue = function
    | Some v -> v
    | None -> defaultValue


(*
  AND composition
  Some + X = X
  None + X = None
  Zero = Some()
  While value is Some, keep computing
*)
[<RequireQualifiedAccess>]
module Maybe =
  let return' x = Some x
  let zero = return' ()
  let bind f x = Option.bind f x
  let map f x = Option.map f x
  let tryWith handler body = try body() with exn -> handler exn
  let tryFinally handler body = try body() finally handler ()
  let using body (disposable: #System.IDisposable) = use d = disposable in body d
  let rec while' body guard =
    match guard() with
    | true -> body() |> bind (fun () -> while' body guard)
    | false -> zero

  type Builder() =
    member _.Zero() = zero
    member _.Return x = return' x
    member _.ReturnFrom x = x
    member _.Delay f = f
    member _.Run f = f()
    member _.Bind(x, fn) = bind fn x
    member _.Combine(x, y) = bind (fun _ -> y) x
    member _.Combine(x, fn) = bind fn x
    member _.TryWith(body, handler) = tryWith handler body
    member _.TryFinally(body, handler) = tryFinally handler body
    member _.Using(disposable, body) = using body disposable
    member _.While(guard, body) = while' body guard


(*
  OR composition
  Some + X = Some
  None + X = X
  Zero = None
  While value is None, keep computing
*)
[<RequireQualifiedAccess>]
module OrElse =
  let return' x = Some x
  let zero = None
  let bind f x = match x with None -> f() | Some x -> Some x
  let map f x = bind (f >> return') x
  let tryWith handler body = try body() with exn -> handler exn
  let tryFinally handler body = try body() finally handler ()
  let using body (disposable: #System.IDisposable) = use d = disposable in body d
  let rec while' body guard =
    match guard() with
    | true -> body() |> bind (fun () -> while' body guard)
    | false -> zero

  type Builder() =
    member _.Zero() = zero
    member _.Yield x = return' x
    member _.YieldFrom x = x
    member _.Delay f = f
    member _.Run f = f()
    member _.Bind(x, fn) = bind fn x
    member _.Combine(x, y) = bind (fun _ -> y) x
    member _.Combine(x, fn) = bind fn x
    member _.TryWith(body, handler) = tryWith handler body
    member _.TryFinally(body, handler) = tryFinally handler body
    member _.Using(disposable, body) = using body disposable
    member _.While(guard, body) = while' body guard

[<AutoOpen>]
module OptionUtils =
  let maybe = Maybe.Builder()
  let or_else = OrElse.Builder()
