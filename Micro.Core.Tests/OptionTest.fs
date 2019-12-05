namespace Micro.Preamble.Tests

open Micro

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type OptionTests() =

  [<Property>]
  member _. ``byDefaultFn should return the option value when it if type Some`` (optValue: int) (defaultValue: int) =
    Option.byDefaultFn (fun() -> defaultValue) (Some optValue) = optValue

  [<Property>]
  member _. ``byDefaultFn should return the default value when the option is None`` (defaultValue: int) =
    Option.byDefaultFn (fun() -> defaultValue) None = defaultValue

  (* Maybe Builder *)

  [<Property>]
  member _. ``maybe with no return is Some()`` () =
    let current = maybe {
      do ()
    }
    current = Some()

  [<Property>]
  member _. ``maybe with return is Some`` (value: int) =
    let current = maybe {
      return value
    }
    current = Some value

  [<Property>]
  member _. ``maybe with return! None is None`` () =
    let current = maybe {
      return! None
    }
    current = None

  [<Property>]
  member _. ``maybe with return! Some is Some`` (value: int) =
    let current = maybe {
      return! Some value
    }
    current = Some value

  [<Property>]
  member _. ``maybe with bind from None is None`` () =
    let current = maybe {
      let! x = None
      let y = x + 1
      return y
    }
    current = None

  [<Property>]
  member _. ``maybe with bind from Some is Some`` (value: int) =
    let current = maybe {
      let! x = Some value
      let y = x + 1
      return y
    }
    current = Some (value + 1)

  [<Property>]
  member _. ``maybe with combine should bind the expressions`` (value: int) =
    let current = maybe {
      do ()
      return value
    }
    current = Some value

  [<Property>]
  member _. ``maybe with if true should return the then branch`` () =
    let current = maybe {
      if true then return! None
    }
    current = None

  [<Property>]
  member _. ``maybe with if false should return None`` () =
    let current = maybe {
      if false then return! None
    }
    current = Some()

  [<Property>]
  member _. ``maybe with if true else should return the then branch`` (value: int) (elseValue: int) =
    let current = maybe {
      if true
      then return value
      else return elseValue
    }
    current = Some value

  [<Property>]
  member _. ``maybe with if false else should return the else branch`` (value: int) (elseValue: int) =
    let current = maybe {
      if false
      then return value
      else return elseValue
    }
    current = Some elseValue

  [<Property>]
  member _. ``maybe with try with normal evaluation should return the evaluation`` (value: int) (exnValue: int) =
    let current = maybe {
      try
        return value
      with _ ->
        return exnValue
    }
    current = Some value

  [<Property>]
  member _. ``maybe with try with failed evaluation should return the handler evaluation`` (value: int) (exnValue: int) =
    let current = maybe {
      try
        invalidOp "Ups, failed"
        return value
      with _ ->
        return exnValue
    }
    current = Some exnValue

  [<Property>]
  member _. ``maybe with try finally with normal evaluation should return the evaluation`` (value: int) =
    let mutable ranFinally = false
    let current = maybe {
      try
        return value
      finally
        ranFinally <- true
    }
    current = Some value && ranFinally

  [<Property>]
  member _. ``maybe with use should run the dispose method`` (value: int) =
    let mutable ranDispose = false
    let disposable =
      { new System.IDisposable with
          member _.Dispose() = ranDispose <- true
      }
    let current = maybe {
      use d = disposable
      return value
    }
    current = Some value && ranDispose

  (* OrElse Builder *)

  [<Property>]
  member _. ``or_else with no return is None`` () =
    let current = or_else {
      do ()
    }
    current = None

  [<Property>]
  member _. ``or_else with yield is Some`` (value: int) =
    let current = or_else {
      yield value
    }
    current = Some value

  [<Property>]
  member _. ``or_else with return! None is None`` () =
    let current = or_else {
      yield! None
    }
    current = None

  [<Property>]
  member _. ``or_else with return! Some is Some`` (value: int) =
    let current = or_else {
      yield! Some value
    }
    current = Some value

  [<Property>]
  member _. ``or_else with bind from None is None`` (value: int) =
    let current = or_else {
      yield! None
      yield! Some value
    }
    current = Some value

  [<Property>]
  member _. ``or_else with bind from Some is Some`` (value1: int) (value2: int) =
    let current = or_else {
      yield! Some value1
      yield! Some value2
    }
    current = Some value1

  [<Property>]
  member _. ``or_else with combine should bind the expressions`` (value: int) =
    let current = or_else {
      do ()
      yield value
    }
    current = Some value

  [<Property>]
  member _. ``or_else with if true should return the then branch`` () =
    let current = or_else {
      if true then yield! None
    }
    current = None

  [<Property>]
  member _. ``or_else with if false should return None`` () =
    let current = or_else {
      if false then yield! Some()
    }
    current = None

  [<Property>]
  member _. ``or_else with if true else should return the then branch`` (value: int) (elseValue: int) =
    let current = or_else {
      if true
      then yield value
      else yield elseValue
    }
    current = Some value

  [<Property>]
  member _. ``or_else with if false else should return the else branch`` (value: int) (elseValue: int) =
    let current = or_else {
      if false
      then yield value
      else yield elseValue
    }
    current = Some elseValue

  [<Property>]
  member _. ``or_else with try with normal evaluation should return the evaluation`` (value: int) (exnValue: int) =
    let current = or_else {
      try
        yield value
      with _ ->
        yield exnValue
    }
    current = Some value

  [<Property>]
  member _. ``or_else with try with failed evaluation should return the handler evaluation`` (value: int) (exnValue: int) =
    let current = or_else {
      try
        invalidOp "Ups, failed"
        yield value
      with _ ->
        yield exnValue
    }
    current = Some exnValue

  [<Property>]
  member _. ``or_else with try finally with normal evaluation should return the evaluation`` (value: int) =
    let mutable ranFinally = false
    let current = or_else {
      try
        yield value
      finally
        ranFinally <- true
    }
    current = Some value && ranFinally

  [<Property>]
  member _. ``or_else with use should run the dispose method`` (value: int) =
    let mutable ranDispose = false
    let disposable =
      { new System.IDisposable with
          member _.Dispose() = ranDispose <- true
      }
    let current = or_else {
      use d = disposable
      yield value
    }
    current = Some value && ranDispose
