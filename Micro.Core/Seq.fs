namespace Micro

open System.Collections

type FoldResult<'t> =
  | Continue
  | ContinueWith of 't
  | Break
  | BreakWith of 't

module Seq =
  let tryBestWithComparison (isBestOrSameThan: 'key -> 'key -> bool) (projection: 'item -> 'key) (source: 'item seq) =
    let folder current newItem =
      let newKey = projection newItem

      match current with
      | Some (maxItem, maxKey) ->
        if isBestOrSameThan maxKey newKey
          then Some (maxItem, maxKey)
          else Some (newItem, newKey)

      | None ->
        Some (newItem, newKey)

    source
    |> Seq.fold folder None
    |> Option.map fst

  let tryMaxWith projection = tryBestWithComparison (>=) projection
  let tryMinWith projection = tryBestWithComparison (<=) projection

  let tryMax sequence = tryMaxWith id sequence
  let tryMin sequence = tryMinWith id sequence

  let ofEnumerable (source: IEnumerable) =
    seq {
      for item in source do
        yield item
    }

  let foldUntil (initState: 'state) (folder: 'state -> 'item -> FoldResult<'state>) (source: 'item seq) =
    use enumerator = source.GetEnumerator()

    let rec loop moved state =
      if not moved then
        state
      else
        match folder state enumerator.Current with
          | Continue -> loop (enumerator.MoveNext()) state
          | ContinueWith state -> loop (enumerator.MoveNext()) state
          | Break -> state
          | BreakWith state -> state
        
    loop (enumerator.MoveNext()) initState

  let foldWhileNone initState folder source =
    let mapper = function None -> Continue | Some x -> BreakWith x
    let folder state value = folder state value |> mapper
    foldUntil initState folder source

  let foldUntilNone initState folder source =
    let mapper = function None -> Break | Some x -> ContinueWith x
    let folder state value = folder state value |> mapper
    foldUntil initState folder source

  let inline foldWhileSome initState folder source = foldUntilNone initState folder source
