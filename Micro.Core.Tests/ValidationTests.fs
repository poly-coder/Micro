namespace Micro.Tests

open Micro
open Micro.Validation

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open System.Text.RegularExpressions

[<TestFixture>]
type ValidationTests() =

  [<Property>]
  member _. ``isNotEmpty with a null string should return validation error`` () =
    let results = isNotEmpty null |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should not be empty")] @>

  [<Property>]
  member _. ``isNotEmpty with an empty string should return validation error`` () =
    let results = isNotEmpty "" |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should not be empty")] @>

  [<Property>]
  member _. ``isNotEmpty with a blank string should not return validation error`` () =
    let results = isNotEmpty "   " |> Seq.toList
    test <@ results = [] @>

  [<Property>]
  member _. ``isNotEmpty with a non-empty string should not return validation error`` (NonEmptyString str: NonEmptyString) =
    let results = isNotEmpty str |> Seq.toList
    test <@ results = [] @>

  [<Property>]
  member _. ``isNotBlank with a null string should return validation error`` () =
    let results = isNotBlank null |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should not be blank")] @>

  [<Property>]
  member _. ``isNotBlank with an Blank string should return validation error`` () =
    let results = isNotBlank "" |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should not be blank")] @>

  [<Property>]
  member _. ``isNotBlank with a blank string should return validation error`` () =
    let results = isNotBlank "   " |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should not be blank")] @>

  [<Property>]
  member _. ``isNotBlank with a non-Blank string should not return validation error`` (NonEmptyString str: NonEmptyString) =
    let results = isNotBlank str |> Seq.toList
    test <@ System.String.IsNullOrWhiteSpace str || results = [] @>

  [<Property>]
  member _. ``hasMinLength with empty string should return validation error`` () =
    let results = hasMinLength 3 "" |> Seq.toList
    test <@ results = [(ValLevel.Error, [], "Should have at least 3 characters")] @>

  [<Property>]
  member _. ``hasMinLength with short string should return validation error`` (NonEmptyString str: NonEmptyString) =
    if String.length str < 3 then
      test <@ hasMinLength 3 str |> Seq.toList = [(ValLevel.Error, [], "Should have at least 3 characters")] @>

  [<Property>]
  member _. ``hasMinLength with long enough string should not return validation error`` (NonEmptyString str: NonEmptyString) =
    if String.length str >= 3 then
      test <@ hasMinLength 3 str |> Seq.toList = [] @>

  [<Property>]
  member _. ``hasMaxLength with empty string should not return validation error`` () =
    let results = hasMaxLength 3 "" |> Seq.toList
    test <@ results = [] @>

  [<Property>]
  member _. ``hasMaxLength with short string should return not validation error`` (NonEmptyString str: NonEmptyString) =
    if String.length str <= 3 then
      test <@ hasMaxLength 3 str |> Seq.toList = [] @>

  [<Property>]
  member _. ``hasMaxLength with long enough string should return validation error`` (NonEmptyString str: NonEmptyString) =
    if String.length str > 3 then
      test <@ hasMaxLength 3 str |> Seq.toList = [(ValLevel.Error, [], "Should have at most 3 characters")] @>

  [<Property>]
  member _. ``hasLength should return validation error if length is not the expected`` (str: string) =
    if String.length str = 3 then
      test <@ hasLength 3 str |> Seq.toList = [] @>
    else
      test <@ hasLength 3 str |> Seq.toList = [(ValLevel.Error, [], "Should have exactly 3 characters")] @>

  [<Property>]
  member _. ``matchesRegex should not return validation error if input does match`` (NonEmptyString str1: NonEmptyString) (NonEmptyString str2: NonEmptyString) (message: string) =
    let str = sprintf "%s@%s" str1 str2
    let regex = Regex("^.+@.+$", RegexOptions.Singleline)
    test <@ matchesRegex message regex str |> Seq.toList = [] @>

  [<Property>]
  member _. ``matchesRegex should return validation error if input does not match`` (NonEmptyString str1: NonEmptyString) (NonEmptyString str2: NonEmptyString) (message: string) =
    let str = sprintf "%s-%s" str1 str2
    let regex = Regex("^.+@.+$", RegexOptions.Singleline)
    if str.Contains "@" |> not then
      test <@ matchesRegex message regex str |> Seq.toList = [(ValLevel.Error, [], message)] @>

  [<Property>]
  member _. ``<&&> should evaluate only the first validator if it fails`` () =
    let validator = isNotEmpty <&&> hasMinLength 3 <&&> hasMaxLength 10
    test <@ validator null |> Seq.toList = [(ValLevel.Error, [], "Should not be empty")] @>

  [<Property>]
  member _. ``<&&> should evaluate the second validator if the first one succeeds`` () =
    let validator = isNotEmpty <&&> hasMinLength 3 <&&> hasMaxLength 10
    test <@ validator "x" |> Seq.toList = [(ValLevel.Error, [], "Should have at least 3 characters")] @>

  [<Property>]
  member _. ``<&&> should evaluate the third validator if the first two succeeds`` () =
    let validator = isNotEmpty <&&> hasMinLength 3 <&&> hasMaxLength 10
    test <@ validator "hello" |> Seq.toList = [] @>

  [<Property>]
  member _. ``<&&> should evaluate the all validators and fail if the last one fails`` () =
    let validator = isNotEmpty <&&> hasMinLength 3 <&&> hasMaxLength 10
    test <@ validator "this is too long!" |> Seq.toList = [(ValLevel.Error, [], "Should have at most 10 characters")] @>

  [<Property>]
  member _. ``<||> should evaluate all validators if the first one fails`` () =
    let validator = hasMaxLength 3 <||> hasMinLength 10
    test <@ validator "medium" |> Seq.toList = [(ValLevel.Error, [], "Should have at least 10 characters")] @>

  [<Property>]
  member _. ``<||> should evaluate only the first validator if it succeeds`` () =
    let validator = hasMaxLength 3 <||> hasMinLength 10
    test <@ validator "x" |> Seq.toList = [] @>

  [<Property>]
  member _. ``<||> should succeed if the second validator succeeds, even if the first one fails`` () =
    let validator = hasMaxLength 3 <||> hasMinLength 10
    test <@ validator "long text should pass" |> Seq.toList = [] @>
