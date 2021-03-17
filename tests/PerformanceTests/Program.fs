module Chess.Domain.PerformanceTests

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

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

    [<Benchmark(Baseline = true)>]
    member this.V1() =
        let data = this.PerftData

        let position = V1.Fen.fromFen data.Fen
        let result = V1.MoveGeneration.perft data.Depth position

        if result <> data.Expected then failwithf "Result not as expected for %O" data

[<EntryPoint>]
let main args =

    let summary = BenchmarkRunner.Run<PerftBenchmarks>()

    0