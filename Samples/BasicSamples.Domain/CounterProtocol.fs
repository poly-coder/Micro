module BasicSamples.Domain.CounterProtocol

open System.IO
open ProtoBuf
open Micro
open BasicSamples.Domain.CounterDomain

let serializePB pb =
  use mem = new MemoryStream()
  Serializer.Serialize (mem, pb)
  mem.ToArray()

let deserializePB (mapper: 'pb -> 'result) bytes =
  use mem = new MemoryStream(bytes: byte[])
  let pb = Serializer.Deserialize<'pb>(mem)
  mapper pb

[<Literal>]
let counterDomain = "basic-samples.domains.counter"
[<Literal>]
let counterCommand = counterDomain + "command"
[<Literal>]
let counterEvent = counterDomain + "event"

module CommandProtocol =

  [<Literal>]
  let startWithName = "start-with"
  let incrementName = "increment"
  let decrementName = "decrement"
  let setToName = "set-to"
  let resetName = "reset"

  [<CLIMutable; ProtoContract>]
  type StartWithPB = {
    [<ProtoMember(1)>]
    start: int
  }

  [<CLIMutable; ProtoContract>]
  type SetToPB = {
    [<ProtoMember(1)>]
    current: int
  }

  let serialize command =
    let typeName, bytes =
      match command with
      | StartWith start ->
        startWithName, serializePB ({ start = start }: StartWithPB)

      | SetTo current ->
        setToName, serializePB ({ current = current }: SetToPB)

      | Increment ->
        incrementName, Array.empty

      | Decrement ->
        decrementName, Array.empty

      | Reset ->
        resetName, Array.empty

    typeName, bytes

  let deserializers =
    [
      incrementName, konst Increment
      decrementName, konst Decrement
      resetName, konst Reset
      startWithName, deserializePB (fun (pb: StartWithPB) -> StartWith pb.start)
      setToName, deserializePB (fun (pb: SetToPB) -> SetTo pb.current)
    ] |> Map.ofSeq

  let deserialize typeName bytes =
    deserializers
    |> Map.tryFind typeName
    |> Option.map (fun fn -> fn bytes)

module EventProtocol =

  [<Literal>]
  let wasStartedWithName = "was-started-with"
  let wasIncrementedName = "was-incremented"
  let wasDecrementedName = "was-decremented"
  let wasSetToName = "was-set-to"
  let wasResetName = "was-reset"

  [<CLIMutable; ProtoContract>]
  type WasStartedWithPB = {
    [<ProtoMember(1)>]
    start: int
  } with
    static member toEvent(pb: WasStartedWithPB) = WasStartedWith pb.start

  [<CLIMutable; ProtoContract>]
  type WasSetToPB = {
    [<ProtoMember(1)>]
    current: int
  } with
    static member toEvent(pb: WasSetToPB) = WasSetTo pb.current

  let serialize event =
    let typeName, bytes =
      match event with
      | WasStartedWith start ->
        wasStartedWithName, serializePB ({ start = start }: WasStartedWithPB)

      | WasSetTo current ->
        wasSetToName, serializePB ({ current = current }: WasSetToPB)

      | WasIncremented ->
        wasIncrementedName, Array.empty

      | WasDecremented ->
        wasDecrementedName, Array.empty

      | WasReset ->
        wasResetName, Array.empty

    typeName, bytes

  let deserializers =
    [
      wasIncrementedName, konst WasIncremented
      wasDecrementedName, konst WasDecremented
      wasResetName, konst WasReset
      wasStartedWithName, deserializePB WasStartedWithPB.toEvent
      wasSetToName, deserializePB WasSetToPB.toEvent
    ] |> Map.ofSeq

  let deserialize typeName bytes =
    deserializers
    |> Map.tryFind typeName
    |> Option.map (fun fn -> fn bytes)

module StateProtocol =

  [<Literal>]
  let stateV1Name = "stateV1"

  [<CLIMutable; ProtoContract>]
  type StateV1PB = {
    [<ProtoMember(1)>]
    start: int
    [<ProtoMember(2)>]
    current: int
  } with
    static member toState(pb: StateV1PB): State =
      { start = pb.start; current = pb.current }
    static member ofState(state: State): StateV1PB =
      { start = state.start; current = state.current }

  let serialize (state: State) =
    stateV1Name, serializePB (StateV1PB.ofState state)

  let deserializers =
    [
      stateV1Name, deserializePB StateV1PB.toState
    ] |> Map.ofSeq

  let deserialize typeName bytes =
    deserializers
    |> Map.tryFind typeName
    |> Option.map (fun fn -> fn bytes)
