namespace Micro.Preamble.Tests

open Micro

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit

[<TestFixture>]
type PreambleTests() =

  [<Property>]
  member _. ``konst should return a function that return k independentlly of the input`` (k: int, input: string) =
    konst k input = k

  [<Property>]
  member _. ``flip should return a flipped argument function`` (a: string, b: string, extra: string) =
      flip (fun x y -> sprintf "%s%s%s" x extra y) a b = sprintf "%s%s%s" b extra a

  [<Property>]
  member _. ``curry should return a curried argument function`` (a: string, b: string, extra: string) =
      curry (fun (x, y) -> sprintf "%s%s%s" x extra y) a b = sprintf "%s%s%s" a extra b

  [<Property>]
  member _. ``uncurry should return an uncurried argument function`` (a: string, b: string, extra: string) =
      uncurry (fun x y -> sprintf "%s%s%s" x extra y) (a, b) = sprintf "%s%s%s" a extra b
