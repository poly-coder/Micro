namespace Micro.Services.Storage.KeyValueStorage

open Micro
open System.Threading

type IKeyValueStore<'key, 'value> =
  abstract Contains: key: 'key -> Async<bool>
  abstract Retrieve: key: 'key -> Async<'value option>
  abstract Store: key: 'key * value: 'value -> Async<unit>
  abstract Remove: key: 'key -> Async<unit>

type KeyValueStore<'key, 'value> =
  {
    contains: 'key -> Async<bool>
    retrieve: 'key -> Async<'value option>
    store: 'key -> 'value -> Async<unit>
    remove: 'key -> Async<unit>
  }

type KeyValueStoreCommand<'key, 'value> =
  | AskContains of 'key * ResultSink<bool, exn>
  | AskRetrieve of 'key * ResultSink<'value option, exn>
  | DoStore of 'key * 'value * ResultSink<unit, exn>
  | DoRemove of 'key * ResultSink<unit, exn>

type KeyValueStoreProcessor<'key, 'value> = KeyValueStoreCommand<'key, 'value> -> unit

module KeyValueStore =
  let interfaceToRecord (instance: IKeyValueStore<'key, 'value>) =
    {
      contains = instance.Contains
      retrieve = instance.Retrieve
      store = curry instance.Store
      remove = instance.Remove
    }

  let recordToInterface (record: KeyValueStore<'key, 'value>) =
    { new IKeyValueStore<'key, 'value> with
        member _.Contains key = record.contains key
        member _.Retrieve key = record.retrieve key
        member _.Store(key, value) = record.store key value
        member _.Remove key = record.remove key
    }

  let recordToProcessor (record: KeyValueStore<'key, 'value>): KeyValueStoreProcessor<'key, 'value> =
    function
    | AskContains(key, replyCh) ->
      replyCh |> ResultSink.tryWith record.contains key |> Async.Start
    | AskRetrieve(key, replyCh) ->
      replyCh |> ResultSink.tryWith record.retrieve key |> Async.Start
    | DoStore(key, value, replyCh) ->
      replyCh |> ResultSink.tryWith2 record.store key value |> Async.Start
    | DoRemove(key, replyCh) ->
      replyCh |> ResultSink.tryWith record.remove key |> Async.Start

  let interfaceToProcessor instance = instance |> (interfaceToRecord >> recordToProcessor)

  let processorToRecord (processor: KeyValueStoreProcessor<'key, 'value>) =
    {
      contains = fun key -> processor |> Sink.asAsync (fun replyCh -> AskContains(key, replyCh))
      retrieve = fun key -> processor |> Sink.asAsync (fun replyCh -> AskRetrieve(key, replyCh))
      store = fun key value -> processor |> Sink.asAsync (fun replyCh -> DoStore(key, value, replyCh))
      remove = fun key -> processor |> Sink.asAsync (fun replyCh -> DoRemove(key, replyCh))
    }

  let processorToInterface processor = processor |> (processorToRecord >> recordToInterface)

  let convertKeys toKey (processor: KeyValueStoreProcessor<_, _>) : KeyValueStoreProcessor<_, _> =
    function
    | AskContains(key, replyCh) -> processor <| AskContains(toKey key, replyCh)
    | AskRetrieve(key, replyCh) -> processor <| AskRetrieve(toKey key, replyCh)
    | DoRemove(key, replyCh) -> processor <| DoRemove(toKey key, replyCh)
    | DoStore(key, value, replyCh) -> processor <| DoStore(toKey key, value, replyCh)

  let convertValues (toValue, fromValue) (processor: KeyValueStoreProcessor<_, _>) : KeyValueStoreProcessor<_, _> =
    function
    | AskContains(key, replyCh) -> processor <| AskContains(key, replyCh)
    | AskRetrieve(key, replyCh) -> processor <| AskRetrieve(key, Result.map (Option.map fromValue) >> replyCh)
    | DoRemove(key, replyCh) -> processor <| DoRemove(key, replyCh)
    | DoStore(key, value, replyCh) -> processor <| DoStore(key, toValue value, replyCh)

  let convert toKey (toValue, fromValue) = convertKeys toKey >> convertValues (toValue, fromValue)
   

  module InMemory =
    module Operations =
      let contains key state =
        state, state |> Map.containsKey key

      let retrieve key state =
        state, state |> Map.tryFind key

      let store key value state =
        state |> Map.add key value, ()

      let remove key state =
        state |> Map.remove key, ()

    open Operations

    let createProcessor (): KeyValueStoreProcessor<'key, 'value> =

      let mailbox = MailboxProcessor.Start(fun mb ->
        let rec loop state = async {
          let! msg = mb.Receive()
          match msg with
          | AskContains(key, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (contains key)

          | AskRetrieve(key, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (retrieve key)

          | DoStore(key, value, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (store key value)

          | DoRemove(key, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (remove key)
        }
        loop Map.empty
      )

      mailbox.Post

type ICancellableKeyValueStore<'key, 'value> =
  abstract Contains: key: 'key * token: CancellationToken -> Async<bool>
  abstract Retrieve: key: 'key * token: CancellationToken -> Async<'value option>
  abstract Store: key: 'key * value: 'value * token: CancellationToken -> Async<unit>
  abstract Remove: key: 'key * token: CancellationToken -> Async<unit>

type CancellableKeyValueStore<'key, 'value> =
  {
    contains: CancellationToken -> 'key -> Async<bool>
    retrieve: CancellationToken -> 'key -> Async<'value option>
    store: CancellationToken -> 'key -> 'value -> Async<unit>
    remove: CancellationToken -> 'key -> Async<unit>
  }

type CancellableKeyValueStoreCommand<'key, 'value> =
  | AskContains of key: 'key * token: CancellationToken * sink: ResultSink<bool, exn>
  | AskRetrieve of key: 'key * token: CancellationToken * sink: ResultSink<'value option, exn>
  | DoStore of key: 'key * value: 'value * token: CancellationToken * sink: ResultSink<unit, exn>
  | DoRemove of key: 'key * token: CancellationToken * sink: ResultSink<unit, exn>

type CancellableKeyValueStoreProcessor<'key, 'value> = CancellableKeyValueStoreCommand<'key, 'value> -> unit

module CancellableKeyValueStore =
  let toNonCancellableRecord (record: CancellableKeyValueStore<'key, 'value>): KeyValueStore<'key, 'value> =
    {
      contains = fun key -> record.contains CancellationToken.None key
      retrieve = fun key -> record.retrieve CancellationToken.None key
      store = fun key value -> record.store CancellationToken.None key value
      remove = fun key -> record.remove CancellationToken.None key
    }

  let toCancellableRecord (record: KeyValueStore<'key, 'value>): CancellableKeyValueStore<'key, 'value> =
    {
      contains = fun _ key -> record.contains key
      retrieve = fun _ key -> record.retrieve key
      store = fun _ key value -> record.store key value
      remove = fun _ key -> record.remove key
    }

  let interfaceToRecord (instance: ICancellableKeyValueStore<'key, 'value>) =
    {
      contains = fun token key -> instance.Contains(key, token)
      retrieve = fun token key -> instance.Retrieve(key, token)
      store = fun token key value -> instance.Store(key, value, token)
      remove = fun token key -> instance.Remove(key, token)
    }

  let recordToInterface (record: CancellableKeyValueStore<'key, 'value>) =
    { new ICancellableKeyValueStore<'key, 'value> with
        member _.Contains(key, token) = record.contains token key
        member _.Retrieve(key, token) = record.retrieve token key
        member _.Store(key, value, token) = record.store token key value
        member _.Remove(key, token) = record.remove token key
    }

  let recordToProcessor (record: CancellableKeyValueStore<'key, 'value>): CancellableKeyValueStoreProcessor<'key, 'value> =
    function
    | AskContains(key, token, replyCh) ->
      replyCh |> ResultSink.tryWith2 record.contains token key |> Async.Start
    | AskRetrieve(key, token, replyCh) ->
      replyCh |> ResultSink.tryWith2 record.retrieve token key |> Async.Start
    | DoStore(key, value, token, replyCh) ->
      replyCh |> ResultSink.tryWith3 record.store token key value |> Async.Start
    | DoRemove(key, token, replyCh) ->
      replyCh |> ResultSink.tryWith2 record.remove token key |> Async.Start

  let interfaceToProcessor instance = instance |> (interfaceToRecord >> recordToProcessor)

  let processorToRecord (processor: CancellableKeyValueStoreProcessor<'key, 'value>) =
    {
      contains = fun token key -> processor |> Sink.asAsync (fun replyCh -> AskContains(key, token, replyCh))
      retrieve = fun token key -> processor |> Sink.asAsync (fun replyCh -> AskRetrieve(key, token, replyCh))
      store = fun token key value -> processor |> Sink.asAsync (fun replyCh -> DoStore(key, value, token, replyCh))
      remove = fun token key -> processor |> Sink.asAsync (fun replyCh -> DoRemove(key, token, replyCh))
    }

  let processorToInterface processor = processor |> (processorToRecord >> recordToInterface)

  let convertKeys toKey (processor: CancellableKeyValueStoreProcessor<_, _>) : CancellableKeyValueStoreProcessor<_, _> =
    function
    | AskContains(key, token, replyCh) -> processor <| AskContains(toKey key, token, replyCh)
    | AskRetrieve(key, token, replyCh) -> processor <| AskRetrieve(toKey key, token, replyCh)
    | DoRemove(key, token, replyCh) -> processor <| DoRemove(toKey key, token, replyCh)
    | DoStore(key, value, token, replyCh) -> processor <| DoStore(toKey key, value, token, replyCh)

  let convertValues (toValue, fromValue) (processor: CancellableKeyValueStoreProcessor<_, _>) : CancellableKeyValueStoreProcessor<_, _> =
    function
    | AskContains(key, token, replyCh) -> processor <| AskContains(key, token, replyCh)
    | AskRetrieve(key, token, replyCh) -> processor <| AskRetrieve(key, token, Result.map (Option.map fromValue) >> replyCh)
    | DoRemove(key, token, replyCh) -> processor <| DoRemove(key, token, replyCh)
    | DoStore(key, value, token, replyCh) -> processor <| DoStore(key, toValue value, token, replyCh)

  let convert toKey (toValue, fromValue) = convertKeys toKey >> convertValues (toValue, fromValue)
   
  module InMemory =
    module Operations =
      let contains token key state =
        Async.throwIfCancellationRequested token
        state, state |> Map.containsKey key

      let retrieve token key state =
        Async.throwIfCancellationRequested token
        state, state |> Map.tryFind key

      let store token key value state =
        Async.throwIfCancellationRequested token
        state |> Map.add key value, ()

      let remove token key state =
        Async.throwIfCancellationRequested token
        state |> Map.remove key, ()

    open Operations

    let createProcessor (): CancellableKeyValueStoreProcessor<'key, 'value> =

      let mailbox = MailboxProcessor.Start(fun mb ->
        let rec loop state = async {
          let! msg = mb.Receive()
          match msg with
          | AskContains(key, token, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (contains token key)

          | AskRetrieve(key, token, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (retrieve token key)

          | DoStore(key, value, token, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (store token key value)

          | DoRemove(key, token, replyCh) ->
            return! state |> Mbox.processMsgSync loop replyCh (remove token key)
        }
        loop Map.empty
      )

      mailbox.Post
  