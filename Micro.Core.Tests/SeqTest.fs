namespace Micro.Tests

open Micro

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type SeqTests() =

  [<Property>]
  member _. ``tryMax should return None when the sequence is empty`` () =
    Seq.tryMax [] = None

  [<Property>]
  member _. ``tryMax should return the maximum when the sequence contains elements`` (NonEmptyArray source: int NonEmptyArray) =
    Seq.tryMax source = Some (Seq.max source)

  [<Property>]
  member _. ``tryMin should return None when the sequence is empty`` () =
    Seq.tryMin [] = None

  [<Property>]
  member _. ``tryMin should return the minimum when the sequence contains elements`` (NonEmptyArray source: int NonEmptyArray) =
    Seq.tryMin source = Some (Seq.min source)

  [<Property>]
  member _. ``tryMaxWith should return None when the sequence is empty`` () =
    Seq.tryMaxWith String.length [] = None

  [<Property>]
  member _. ``tryMaxWith should return the maximum when the sequence contains elements`` (NonEmptyArray source: NonEmptyString NonEmptyArray) =
    let source = source |> Seq.map (fun s -> s.Get) |> Seq.toList
    match source |> Seq.tryMaxWith String.length with
    | Some longest -> source |> List.forall (fun str -> str.Length <= longest.Length)
    | None -> false

  [<Property>]
  member _. ``tryMinWith should return None when the sequence is empty`` () =
    Seq.tryMinWith String.length [] = None

  [<Property>]
  member _. ``tryMinWith should return the minimum when the sequence contains elements`` (NonEmptyArray source: NonEmptyString NonEmptyArray) =
    let source = source |> Seq.map (fun s -> s.Get) |> Seq.toList
    match source |> Seq.tryMinWith String.length with
    | Some longest -> source |> List.forall (fun str -> str.Length >= longest.Length)
    | None -> false
