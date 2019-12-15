namespace Micro

module Result =
  let tryWith fn = try Ok (fn()) with exn -> Error exn
