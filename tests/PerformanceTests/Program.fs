module Chess.Domain.PerformanceTests

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Dummy() =
    [<Benchmark>]
    member this.Slow() = Threading.Thread.Sleep(100)

    [<Benchmark>]
    member this.Fast() = Threading.Thread.Sleep(50)

[<EntryPoint>]
let main args =

    let summary = BenchmarkRunner.Run<Dummy>()

    0