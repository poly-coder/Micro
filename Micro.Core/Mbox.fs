namespace Micro

module Mbox =

  let processMsg loop replyCh operationFn state = async {
    try
      let! state', response = operationFn state
      replyCh <| Ok response
      return! loop state'
    with exn ->
      replyCh <| Error exn
      return! loop state
  }

  let processMsgSync loop replyCh operationFn state =
    try
      let state', response = operationFn state
      replyCh <| Ok response
      loop state'
    with exn ->
      replyCh <| Error exn
      loop state
