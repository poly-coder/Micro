namespace Micro

open System.Threading
open System.Threading.Tasks

module Async =
  let throwIfCancellationRequested (token: CancellationToken) = token.ThrowIfCancellationRequested()
  let canBeCanceled (token: CancellationToken) = token.CanBeCanceled
  let isCancellationRequested (token: CancellationToken) = token.IsCancellationRequested

type Sink<'t> = 't -> unit

module Sink =
  let asAsync commandFn (sink: Sink<_>) =
    let source = TaskCompletionSource()
    let replyCh = function
      | Ok v -> source.TrySetResult v |> ignore
      | Error exn -> source.TrySetException(exn: exn) |> ignore
    let command = commandFn replyCh
    do sink command
    source.Task |> Async.AwaitTask


type ResultSink<'t, 'e> = Sink<Result<'t, 'e>>

module ResultSink =
  let tryWith0 fn replyCh = async {
    try
      let! result = fn ()
      replyCh (Ok result)
    with
      exn -> replyCh (Error exn)
  }

  let tryWithValue asyncValue =
    tryWith0 (fun () -> asyncValue)

  let tryWith fn arg =
    tryWith0 (fun () -> fn arg)

  let tryWith2 fn arg1 arg2 =
    tryWith0 (fun () -> fn arg1 arg2)

  let tryWith3 fn arg1 arg2 arg3 =
    tryWith0 (fun () -> fn arg1 arg2 arg3)

  let tryWith4 fn arg1 arg2 arg3 arg4 =
    tryWith0 (fun () -> fn arg1 arg2 arg3 arg4)
