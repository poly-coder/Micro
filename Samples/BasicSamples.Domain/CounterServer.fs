module BasicSamples.Domain.CounterServer

open System
open Micro
open BasicSamples.Domain.CounterDomain
open BasicSamples.Domain.CounterProtocol

module InMemory =
  type ServerRequest =
    | CounterCommand of entityId: string * command: Command * AsyncReplyChannel<Event list>

  let createServer () =
    let mailbox = MailboxProcessor.Start(fun inbox ->
      let rec loop state = async {
        printfn ""
        let! message = inbox.Receive()
        match message with
        | CounterCommand(entityId, command, ch) ->
          let counter, state' =
            match state |> Map.tryFind entityId with
            | Some counter -> counter, state
            | None ->
              let counter = initState
              let state = state |> Map.add entityId counter
              counter, state

          let events = handle counter command
          ch.Reply(events)

          match events with
          | [] ->
            return! loop state
          | _ ->
            let counter' = events |> Seq.fold apply counter
            let state' = state' |> Map.add entityId counter'
            printfn "SERVER: Counter [%s: %A] received command [%A] emitted events [%A] and remained as [%A]" entityId counter command events counter'
            return! loop state'
      }      
      loop Map.empty
    )

    mailbox
