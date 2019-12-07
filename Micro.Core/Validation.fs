module Micro.Validation

open Micro

type ValItem<'level, 'step, 'message> = 'level * 'step list * 'message

module TypedValItem =
  let create level message: ValItem<_, _, _> = level, [], message

  let getLevel (level, _, _) = level
  let getPath (_, path, _) = path
  let getMessage (_, _, message) = message

  let withLevel level (_, path, message) : ValItem<_, _, _> = level, path, message
  let withPath path (level, _, message) : ValItem<_, _, _> = level, path, message
  let withSteps steps item = item |> withPath (steps @ getPath item)
  let withStep step item = withSteps [step] item
  let withMessage message (level, path, _) : ValItem<_, _, _> = level, path, message

type ValResult<'level, 'step, 'message> = ValItem<'level, 'step, 'message> seq

module TypedValResult =
  let empty: ValResult<_, _, _> = Seq.empty

  let getOnly level = Seq.filter (fun (item: ValItem<_, _, _>) -> TypedValItem.getLevel item = level)
  
  let getAtLeast level = Seq.filter (fun (item: ValItem<_, _, _>) -> TypedValItem.getLevel item >= level)
  let getAtMost level = Seq.filter (fun (item: ValItem<_, _, _>) -> TypedValItem.getLevel item <= level)

  let tryMaxLevel results = results |> Seq.tryMaxWith TypedValItem.getLevel |> Option.map TypedValItem.getLevel
  let tryMinLevel results = results |> Seq.tryMinWith TypedValItem.getLevel |> Option.map TypedValItem.getLevel

  let asLevel level messages = messages |> Seq.map (fun message -> TypedValItem.create level message)
  
  let withLevel level = Seq.map (TypedValItem.withLevel level)
  let withPath path = Seq.map (TypedValItem.withPath path)
  let withSteps steps = Seq.map (TypedValItem.withSteps steps)
  let withStep step = Seq.map (TypedValItem.withStep step)
  let withMessage message = Seq.map (TypedValItem.withMessage message)
  
  let mapMessages messageFn = Seq.map (fun item -> TypedValItem.withMessage (messageFn (TypedValItem.getMessage item)) item)

type Validator<'value, 'level, 'step, 'message> = 'value -> ValResult<'level, 'step, 'message>
type AsyncValidator<'value, 'level, 'step, 'message> = 'value -> Async<ValResult<'level, 'step, 'message>>

module TypedValidator =
  let ofPredicate condFn messageFn =
    fun value -> seq {
      if condFn value |> not then
        yield messageFn value
    }

(*
  Specific implementation from common level and step types, and message as string
*)

type ValLevel =
  | None = 0
  | Hint = 1
  | Warn = 2
  | Error = 3
  | Fatal = 4

type ValStep =
  | PropertyStep of string
  | IndexStep of int

type ValItem<'message> = ValItem<ValLevel, ValStep, 'message>

module ValItem =
  let create level message : ValItem<_> = TypedValItem.create level message
  let empty level = create level ""

  let none message = create ValLevel.None message
  let hint message = create ValLevel.Hint message
  let warn message = create ValLevel.Warn message
  let error message = create ValLevel.Error message
  let fatal message = create ValLevel.Fatal message

  let getLevel (item: ValItem<_>) = TypedValItem.getLevel item
  let getPath (item: ValItem<_>) = TypedValItem.getPath item
  let getMessage (item: ValItem<_>) = TypedValItem.getMessage item

  let withLevel level (item: ValItem<_>) = TypedValItem.withLevel level item
  let withPath path (item: ValItem<_>) = TypedValItem.withPath path item
  let withSteps steps (item: ValItem<_>) = TypedValItem.withSteps steps item
  let withStep step (item: ValItem<_>) = TypedValItem.withStep step item
  let withMessage message (item: ValItem<_>) = TypedValItem.withMessage message item

  let withProp propertyName = withStep (PropertyStep propertyName)
  let withIndex index = withStep (IndexStep index)
  let withCollection propertyName index = withIndex index >> withProp propertyName

  let pathToJson path = 
    let rec loop path needsDot result =
      match path with
      | [] -> result |> List.rev

      | head :: tail ->
        match head with
        | PropertyStep name ->
          let result' = name :: result
          let result' = if needsDot then "." :: result' else result'
          loop tail true result'

        | IndexStep index ->
          let result' = (sprintf "[%d]" index) :: result
          loop tail false result'
    
    loop path false []
    |> String.concat ""

type ValResult<'message> = ValResult<ValLevel, ValStep, 'message>

module ValResult =
  let empty: ValResult<_> = Seq.empty

  let getOnly level = Seq.filter (fun item -> ValItem.getLevel item = level)
  
  let getOnlyNone result = getOnly ValLevel.None result
  let getOnlyHint result = getOnly ValLevel.Hint result
  let getOnlyWarn result = getOnly ValLevel.Warn result
  let getOnlyError result = getOnly ValLevel.Error result
  let getOnlyFatal result = getOnly ValLevel.Fatal result
  
  let getAtLeast level = Seq.filter (fun item -> ValItem.getLevel item >= level)
  let getAtMost level = Seq.filter (fun item -> ValItem.getLevel item <= level)
  
  let getAtLeastNone result = getAtLeast ValLevel.None result
  let getAtLeastHint result = getAtLeast ValLevel.Hint result
  let getAtLeastWarn result = getAtLeast ValLevel.Warn result
  let getAtLeastError result = getAtLeast ValLevel.Error result
  let getAtLeastFatal result = getAtLeast ValLevel.Fatal result
  
  let getAtMostNone result = getAtMost ValLevel.None result
  let getAtMostHint result = getAtMost ValLevel.Hint result
  let getAtMostWarn result = getAtMost ValLevel.Warn result
  let getAtMostError result = getAtMost ValLevel.Error result
  let getAtMostFatal result = getAtMost ValLevel.Fatal result

  let tryMaxLevel results = results |> Seq.tryMaxWith ValItem.getLevel |> Option.map ValItem.getLevel
  let tryMinLevel results = results |> Seq.tryMinWith ValItem.getLevel |> Option.map ValItem.getLevel

  let maxLevel results = tryMaxLevel results |> Option.defaultValue ValLevel.None

  let asLevel level messages : ValResult<_> = TypedValResult.asLevel level messages

  let asNone messages = asLevel ValLevel.None messages
  let asHint messages = asLevel ValLevel.Hint messages
  let asWarn messages = asLevel ValLevel.Warn messages
  let asError messages = asLevel ValLevel.Error messages
  let asFatal messages = asLevel ValLevel.Fatal messages

  let withLevel level items : ValResult<_> = TypedValResult.withLevel level items

  let withNone items = withLevel ValLevel.None items
  let withHint items = withLevel ValLevel.Hint items
  let withWarn items = withLevel ValLevel.Warn items
  let withError items = withLevel ValLevel.Error items
  let withFatal items = withLevel ValLevel.Fatal items
  
  let mapMessages messageFn results : ValResult<_> = TypedValResult.mapMessages messageFn results

  let withPath path (items: ValResult<_>): ValResult<_> = TypedValResult.withPath path items
  let withSteps steps (items: ValResult<_>): ValResult<_> = TypedValResult.withSteps steps items
  let withStep step (items: ValResult<_>): ValResult<_> = TypedValResult.withStep step items
  let withMessage message (items: ValResult<_>): ValResult<_> = TypedValResult.withMessage message items

  let withProp propertyName = withStep (PropertyStep propertyName)
  let withIndex index = withStep (IndexStep index)
  let withCollection propertyName index = withIndex index >> withProp propertyName

type Validator<'value, 'message> = 'value -> ValResult<'message>
type Validator<'value> = Validator<'value, string>
type AsyncValidator<'value, 'message> = 'value -> Async<ValResult<'message>>
type AsyncValidator<'value> = AsyncValidator<'value, string>

module Validator =
  let ofCond condFn messageFn =
    fun value -> seq {
      if condFn value |> not then
        yield messageFn value
    }

  let allValidationsPass maxAllowedLevel (validators: Validator<_> list) =
    fun x ->
      let rec loop validators accum =
        match validators with
        | [] -> accum
        | head :: tail ->
          let asList = head x |> Seq.toList
          let level = ValResult.maxLevel asList
          if level > maxAllowedLevel
            then accum @ asList
            else loop tail (accum @ asList)

      loop validators [] |> List.toSeq

  let someValidationPass maxAllowedLevel (validators: Validator<_> list) =
    fun x ->
      let rec loop validators accum =
        match validators with
        | [] -> accum
        | head :: tail ->
          let asList = head x |> Seq.toList
          let level = ValResult.maxLevel asList
          if level > maxAllowedLevel
            then loop tail asList
            else asList

      loop validators [] |> List.toSeq

  let asError messageFn validator = validator messageFn >> ValResult.asError

  let noErrors _ = ValResult.empty

  let isNotEmptyCond = System.String.IsNullOrEmpty >> not
  let isNotEmptyWith messageFn = ofCond isNotEmptyCond messageFn
  let isNotEmpty = isNotEmptyWith |> asError (fun _ -> "Should not be empty")

  let isNotBlankCond = System.String.IsNullOrWhiteSpace >> not
  let isNotBlankWith messageFn = ofCond isNotBlankCond messageFn
  let isNotBlank = isNotBlankWith |> asError (fun _ -> "Should not be blank")

  let hasMinLengthCond minLength = fun str -> String.length str >= minLength
  let hasMinLengthWith minLength = ofCond (hasMinLengthCond minLength)
  let hasMinLength minLength = hasMinLengthWith minLength |> asError (fun _ -> sprintf "Should have at least %d characters" minLength)

  let hasMaxLengthCond maxLength = fun str -> String.length str <= maxLength
  let hasMaxLengthWith maxLength = ofCond (hasMaxLengthCond maxLength)
  let hasMaxLength maxLength = hasMaxLengthWith maxLength |> asError (fun _ -> sprintf "Should have at most %d characters" maxLength)

  let hasLengthCond length = fun str -> String.length str = length
  let hasLengthWith length = ofCond (hasLengthCond length)
  let hasLength length = hasLengthWith length |> asError (fun _ -> sprintf "Should have exactly %d characters" length)

  let matchesRegexCond (regex: System.Text.RegularExpressions.Regex) = fun str -> regex.IsMatch str
  let matchesRegexWith regex = ofCond (matchesRegexCond regex)
  let matchesRegex message regex = matchesRegexWith regex |> asError (fun _ -> message)

  let isEqualToCond value = (=) value
  let isEqualToWith value messageFn = ofCond (isEqualToCond value) messageFn
  let isEqualTo value = isEqualToWith value |> asError (fun _ -> sprintf "Should be equal to %O" value)

  let isNotEqualToCond value = isEqualToCond value >> not
  let isNotEqualToWith value messageFn = ofCond (isNotEqualToCond value) messageFn
  let isNotEqualTo value = isNotEqualToWith value |> asError (fun _ -> sprintf "Should not be equal to %O" value)

  let isGreaterThanCond value = fun x -> x > value
  let isGreaterThanWith value messageFn = ofCond (isGreaterThanCond value) messageFn
  let isGreaterThan value = isGreaterThanWith value |> asError (fun _ -> sprintf "Should be greater than %O" value)

  let isGreaterThanOrEqualToCond value = fun x -> x >= value
  let isGreaterThanOrEqualToWith value messageFn = ofCond (isGreaterThanOrEqualToCond value) messageFn
  let isGreaterThanOrEqualTo value = isGreaterThanOrEqualToWith value |> asError (fun _ -> sprintf "Should be greater than or equal to %O" value)

  let isNotGreaterThanCond value = isGreaterThanCond value >> not
  let isNotGreaterThanWith value messageFn = ofCond (isNotGreaterThanCond value) messageFn
  let isNotGreaterThan value = isNotGreaterThanWith value |> asError (fun _ -> sprintf "Should not be greater than %O" value)

  let isNotGreaterThanOrEqualToCond value = isGreaterThanOrEqualToCond value >> not
  let isNotGreaterThanOrEqualToWith value messageFn = ofCond (isNotGreaterThanOrEqualToCond value) messageFn
  let isNotGreaterThanOrEqualTo value = isGreaterThanOrEqualToWith value |> asError (fun _ -> sprintf "Should not be greater than or equal to %O" value)

  let isLessThanCond value = fun x -> x < value
  let isLessThanWith value messageFn = ofCond (isLessThanCond value) messageFn
  let isLessThan value = isLessThanWith value |> asError (fun _ -> sprintf "Should be less than %O" value)

  let isLessThanOrEqualToCond value = fun x -> x <= value
  let isLessThanOrEqualToWith value messageFn = ofCond (isLessThanOrEqualToCond value) messageFn
  let isLessThanOrEqualTo value = isLessThanOrEqualToWith value |> asError (fun _ -> sprintf "Should be less than or equal to %O" value)

  let isNotLessThanCond value = isLessThanCond value >> not
  let isNotLessThanWith value messageFn = ofCond (isNotLessThanCond value) messageFn
  let isNotLessThan value = isNotLessThanWith value |> asError (fun _ -> sprintf "Should not be less than %O" value)

  let isNotLessThanOrEqualToCond value = isLessThanOrEqualToCond value >> not
  let isNotLessThanOrEqualToWith value messageFn = ofCond (isNotLessThanOrEqualToCond value) messageFn
  let isNotLessThanOrEqualTo value = isLessThanOrEqualToWith value |> asError (fun _ -> sprintf "Should not be less than or equal to %O" value)

  let isBetweenCond minValue maxValue = fun x -> minValue <= x && x <= maxValue
  let isBetweenWith minValue maxValue messageFn = ofCond (isBetweenCond minValue maxValue) messageFn
  let isBetween minValue maxValue = isBetweenWith minValue maxValue |> asError (fun _ -> sprintf "Should be between %O and %O" minValue maxValue)

  let isNotBetweenCond minValue maxValue = isBetweenCond minValue maxValue >> not
  let isNotBetweenWith minValue maxValue messageFn = ofCond (isNotBetweenCond minValue maxValue) messageFn
  let isNotBetween minValue maxValue = isNotBetweenWith minValue maxValue |> asError (fun _ -> sprintf "Should not be between %O and %O" minValue maxValue)

(*
  Common functions
*)

let inline pathToJson path = ValItem.pathToJson path

let inline getOnlyNone result = ValResult.getOnlyNone result
let inline getOnlyHint result = ValResult.getOnlyHint result
let inline getOnlyWarn result = ValResult.getOnlyWarn result
let inline getOnlyError result = ValResult.getOnlyError result
let inline getOnlyFatal result = ValResult.getOnlyFatal result

let inline getAtLeastNone result = ValResult.getAtLeastNone result
let inline getAtLeastHint result = ValResult.getAtLeastHint result
let inline getAtLeastWarn result = ValResult.getAtLeastWarn result
let inline getAtLeastError result = ValResult.getAtLeastError result
let inline getAtLeastFatal result = ValResult.getAtLeastFatal result

let inline getAtMostNone result = ValResult.getAtMostNone result
let inline getAtMostHint result = ValResult.getAtMostHint result
let inline getAtMostWarn result = ValResult.getAtMostWarn result
let inline getAtMostError result = ValResult.getAtMostError result
let inline getAtMostFatal result = ValResult.getAtMostFatal result

let inline tryMaxLevel results = ValResult.tryMaxLevel results
let inline tryMinLevel results = ValResult.tryMinLevel results

let inline maxLevel results = ValResult.maxLevel results

let inline asNone messages = ValResult.asNone messages
let inline asHint messages = ValResult.asHint messages
let inline asWarn messages = ValResult.asWarn messages
let inline asError messages = ValResult.asError messages
let inline asFatal messages = ValResult.asFatal messages

let inline withNone items = ValResult.withNone items
let inline withHint items = ValResult.withHint items
let inline withWarn items = ValResult.withWarn items
let inline withError items = ValResult.withError items
let inline withFatal items = ValResult.withFatal items

let inline mapMessages messageFn results = ValResult.mapMessages messageFn results

let inline withPath path items = ValResult.withPath path items
let inline withSteps steps items = ValResult.withSteps steps items
let inline withStep step items = ValResult.withStep step items
let inline withMessage message items = ValResult.withMessage message items

let inline withProp propertyName = ValResult.withProp propertyName
let inline withIndex index = ValResult.withIndex index
let inline withCollection propertyName index = ValResult.withCollection propertyName index

let noErrors = Validator.noErrors

let isNotEmpty = Validator.isNotEmpty
let isNotBlank = Validator.isNotBlank
let hasMinLength = Validator.hasMinLength
let hasMaxLength = Validator.hasMaxLength
let hasLength = Validator.hasLength
let matchesRegex = Validator.matchesRegex

let isGreaterThan = Validator.isGreaterThan
let isGreaterThanOrEqualTo = Validator.isGreaterThanOrEqualTo
let isNotGreaterThan = Validator.isNotGreaterThan
let isNotGreaterThanOrEqualTo = Validator.isNotGreaterThanOrEqualTo
let isLessThan = Validator.isLessThan
let isLessThanOrEqualTo = Validator.isLessThanOrEqualTo
let isNotLessThan = Validator.isNotLessThan
let isNotLessThanOrEqualTo = Validator.isNotLessThanOrEqualTo
let isBetween = Validator.isBetween
let isNotBetween = Validator.isNotBetween

let (<&&>) left right = Validator.allValidationsPass ValLevel.Warn [left; right]
let (<||>) left right = Validator.someValidationPass ValLevel.Warn [left; right]
