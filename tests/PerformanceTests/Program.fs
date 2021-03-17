module Chess.Domain.PerformanceTests

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Chess.Domain.Fen
open Chess.Domain.MoveGeneration

type PerftTestData =
    {
        Name: string
        Fen: string
        Depth: int
        Expected: int
    }
    override this.ToString() = sprintf "%s, depth %i" this.Name this.Depth

[<MemoryDiagnoser>]
type PerftBenchmarks() =

    member public this.GetData =
        [
            { Name = "Initial position"; Fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; Depth = 4; Expected = 197281 }
        ]

    [<DefaultValue>]
    [<ParamsSource("GetData")>]
    val mutable PerftData : PerftTestData

    [<Benchmark>]
    member this.Original() =
        let data = this.PerftData

        let position = fromFen data.Fen
        let result = perft data.Depth position

        if result <> data.Expected then failwithf "Result not as expected for %O" data

[<EntryPoint>]
let main args =

    let summary = BenchmarkRunner.Run<PerftBenchmarks>()

    0