module BasicSamples.Domain.CounterDomain

open Micro
open Micro.Validation

type Command =
  | StartWith of int
  | Increment
  | Decrement
  | SetTo of int
  | Reset

type Event =
  | WasStartedWith of int
  | WasIncremented
  | WasDecremented
  | WasReset
  | WasSetTo of int

type State =
  {
    start: int
    current: int
  }

let initState = {
  start = 0
  current = 0
}

let handle state command = 
  match command with
  | StartWith start ->
    [ WasStartedWith start; WasSetTo start ]

  | Increment ->
    [ WasIncremented; WasSetTo (state.current + 1) ]

  | Decrement ->
    [ WasDecremented; WasSetTo (state.current - 1) ]

  | SetTo value ->
    [ WasSetTo value ]

  | Reset ->
    [ WasReset; WasSetTo state.start ]

let apply state event =
  match event with
  | WasStartedWith start ->
    { state with start = start }

  | WasSetTo current -> 
    { state with current = current }

  | _ -> state