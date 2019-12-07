module Program

open System
open System.IO
open MongoDB.Bson
open MongoDB.Driver
open NATS.Client
open NATS.Client.Rx
open STAN.Client
open STAN.Client.Rx
open System
open BasicSamples.Domain.CounterDomain
open BasicSamples.Domain.CounterProtocol
open BasicSamples.Domain.CounterServer.InMemory
open ProtoBuf
open System.Threading

let natsUrl = "nats://localhost:4222"

[<CLIMutable; ProtoContract>]
type MessageDataPB = {
  [<ProtoMember(1)>]
  typeName: string

  [<ProtoMember(2)>]
  content: byte[]
}

[<CLIMutable; ProtoContract>]
type CommandRequestPB = {
  [<ProtoMember(1)>]
  entityId: string

  [<ProtoMember(2)>]
  command: MessageDataPB
}

[<CLIMutable; ProtoContract>]
type EventsResponsePB = {
  [<ProtoMember(1)>]
  entityId: string

  [<ProtoMember(2)>]
  events: ResizeArray<MessageDataPB>
}

let createConnection name =
  let opts = ConnectionFactory.GetDefaultOptions()
  opts.AllowReconnect <- true
  opts.Url <- natsUrl
  opts.MaxReconnect <- 60
  opts.ReconnectWait <- 1000
  opts.Name <- name
  let factory = new ConnectionFactory()
  let conn = factory.CreateConnection(opts)
  conn

let startCounterServer (cancel: CancellationToken) = async {
  let conn = createConnection "server"

  let counterProcessor = createServer()

  printfn "Starting server subscription"

  let subscription = conn.Observe(counterCommand).Subscribe(fun msg ->
    if msg.Data <> null then
      let message = deserializePB id<CommandRequestPB> msg.Data
      match CommandProtocol.deserialize message.command.typeName message.command.content with
      | Some cmd ->
        async {
          let! events = counterProcessor.PostAndAsyncReply(fun ch -> CounterCommand(message.entityId, cmd, ch))
          if msg.Reply |> String.IsNullOrEmpty |> not then
            let events =
              events
              |> Seq.map (fun e -> EventProtocol.serialize e)
              |> Seq.map (fun (typeName, content) -> { typeName = typeName; content = content })
              |> ResizeArray
            let response = { events = events; entityId = message.entityId }
            msg.Respond(serializePB response)
        } |> Async.Start
      | None ->
        printfn "Received unknown counter command: %s" message.command.typeName
  )

  let rec waitLoop() = async {
    if cancel.IsCancellationRequested then
      printfn "Disposing server subscription"
      subscription.Dispose()
    else
      do! Async.Sleep(100)
      return! waitLoop()
  }

  printfn "Starting wait loop"
  waitLoop() |> Async.Start
}

let sendCounterRequest (conn: IConnection) entityId command = async {
  let typeName, content = CommandProtocol.serialize command
  printfn "CLIENT: Sending [%s: %A]" entityId command
  let! response = conn.RequestAsync(counterCommand, serializePB { command = { typeName = typeName; content = content }; entityId = entityId }) |> Async.AwaitTask
  let events = deserializePB (fun (pb: EventsResponsePB) -> pb.events) response.Data
  let events = events |> Seq.map (fun pb -> EventProtocol.deserialize pb.typeName pb.content |> Option.get) |> Seq.toList
  printfn "CLIENT: Received [%s: %A] => %A" entityId command events
  printfn ""
  return events
}

let startClient () = async {
  use conn = createConnection "client"

  let! _ = sendCounterRequest conn "1" (StartWith 5)
  let! _ = sendCounterRequest conn "1" (Increment)
  let! _ = sendCounterRequest conn "1" (Increment)
  let! _ = sendCounterRequest conn "1" (Increment)
  let! _ = sendCounterRequest conn "1" (Decrement)
  let! _ = sendCounterRequest conn "1" (SetTo -1)
  let! _ = sendCounterRequest conn "1" (Reset)
  
  return ()
}

let [<EntryPoint>] main _ =
  printfn "Welcome to Counter Server"

  printfn ""
  printfn "%s" (Serializer.GetProto<EventsResponsePB>())
  printfn ""
  use cancelSource = new CancellationTokenSource()

  startCounterServer(cancelSource.Token) |> Async.Start
  printfn "Counter Server started!"

  printfn "Type any key to start the client..."
  Console.ReadKey() |> ignore

  startClient() |> Async.Start  

  printfn "Type any key to stop the server..."
  Console.ReadKey() |> ignore

  cancelSource.Cancel()
  0
