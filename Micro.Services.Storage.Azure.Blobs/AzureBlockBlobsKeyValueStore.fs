namespace Micro.Services.Storage.Azure.Blobs

open Micro.Services.Storage.KeyValueStorage
open Microsoft.Azure.Storage
open Microsoft.Azure.Storage.Blob
open FSharp.Control.Tasks.V2
open System.Threading
open System.Threading.Tasks

type AzureBlobsKeyValueStoreOptions =
  {
    getBlobContainer: CancellationToken -> Task<CloudBlobContainer>
  }

module AzureBlockBlobsKeyValueStore =
  let create options: CancellableKeyValueStore<string, byte[]> =    
    let contains token key =
      task {
        let! container = options.getBlobContainer(token)
        let blob = container.GetBlockBlobReference(key)
        let! exists = blob.ExistsAsync()
        return exists
      } |> Async.AwaitTask

    let retrieve token key =
      task {
        let! container = options.getBlobContainer(token)
        let blob = container.GetBlockBlobReference(key)
        try
          do! blob.FetchAttributesAsync(token)
          let length = int blob.Properties.Length
          let bytes = Array.zeroCreate length
          let! _ = blob.DownloadToByteArrayAsync(bytes, 0, token)
          return Some bytes
        with
        | :? StorageException as ex ->
          if not (isNull ex.RequestInformation) && ex.RequestInformation.HttpStatusCode = 404 then
            return None
          else
            return raise ex
        | exn ->
            return raise exn
      } |> Async.AwaitTask

    let store token key value =
      task {
        let! container = options.getBlobContainer(token)
        let blob = container.GetBlockBlobReference(key)
        do! blob.UploadFromByteArrayAsync(value, 0, value.Length, token)
        return ()
      } |> Async.AwaitTask

    let remove token key =
      task {
        let! container = options.getBlobContainer(token)
        let blob = container.GetBlockBlobReference(key)
        let! _ = blob.DeleteIfExistsAsync(token)
        return ()
      } |> Async.AwaitTask

    {
      contains = contains
      retrieve = retrieve
      store = store
      remove = remove
    }