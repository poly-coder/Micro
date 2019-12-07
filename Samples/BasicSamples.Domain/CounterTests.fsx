#I @"C:/Users/iskan/.nuget/packages/"
#I @"bin/Debug/netstandard2.0/"
#r "protobuf-net/2.4.4/lib/netstandard2.0/protobuf-net.dll"
#r "mongodb.bson/2.9.3/lib/netstandard1.5/MongoDB.Bson.dll"
#r "mongodb.driver.core/2.9.3/lib/netstandard1.5/MongoDB.Driver.Core.dll"
#r "mongodb.driver/2.9.3/lib/netstandard1.5/MongoDB.Driver.dll"
#r "nats.client/0.10.0/lib/netstandard1.6/NATS.Client.dll"
#r "stan.client/0.2.0/lib/netstandard1.6/STAN.Client.dll"
#r "Micro.Core.dll"
#r "BasicSamples.Domain.dll"

open System
open MongoDB.Bson
open MongoDB.Driver
open NATS.Client
open NATS.Client.Rx
open STAN.Client
open STAN.Client.Rx
open System
open BasicSamples.Domain.CounterDomain
open BasicSamples.Domain.CounterProtocol

let natsUrl = "nats://localhost:4222"

let createConnection () =
  let opts = ConnectionFactory.GetDefaultOptions()
  opts.AllowReconnect <- true
  opts.Url <- natsUrl
  opts.MaxReconnect <- 60
  opts.ReconnectWait <- 1000
  let factory = new ConnectionFactory()
  let conn = factory.CreateConnection(opts)
  conn


let startCounterServer() = async {
  use conn = createConnection()
  return 0
}

printfn "Hello"
