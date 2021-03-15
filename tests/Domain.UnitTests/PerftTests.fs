module Chess.Domain.UnitTests.PerftTests

open Expecto

open Chess.Domain.Fen
open Chess.Domain.Position
open Chess.Domain.Move
open Chess.Domain.MoveGeneration

// see https://www.chessprogramming.org/Perft_Results

let perftTestPosition1 = "Initial position", getInitialPosition()
let perftTestPosition2 = "Position 2", fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
let perftTestPosition3 = "Position 3", fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
let perftTestPosition4 = "Position 4", fromFen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
let perftTestPosition4Mirrored = "Position 4 mirrored", fromFen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
let perftTestPosition5 = "Position 5", fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
let perftTestPosition6 = "Position 6", fromFen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")

let perftTestData =
    [
        perftTestPosition1, [ 20; 400; 8902; 197281 ]
        perftTestPosition2, [ 48; 2039; 97862 ]
        perftTestPosition3, [ 14; 191; 2812; 43238 ]
        perftTestPosition4, [ 6; 264; 9467 ]
        perftTestPosition4Mirrored, [ 6; 264; 9467 ]
        perftTestPosition5, [ 44; 1486; 62379 ]
        perftTestPosition6, [ 46; 2079; 89890 ]
    ]

let slowPerfTestData =
    [
        perftTestPosition1, [ 5, 4865609 ]
        perftTestPosition2, [ 4, 4085603 ]
        perftTestPosition3, [ 5, 674624; 6, 11030083 ]
        perftTestPosition4, [ 4, 422333; 5, 15833292 ]
        perftTestPosition4Mirrored, [ 4, 422333 ]
        perftTestPosition5, [ 4, 2103487 ]
        perftTestPosition6, [ 4, 3894594 ]        
    ]

let testPerft name position depth expectedPerft =
    test (sprintf "Depth %i" depth) {
        let result = perft depth position

        Expect.equal result expectedPerft (sprintf "Perft depth %i for %s should be %i" depth name expectedPerft)
    }

let testPerftList ((name, position), expectedResults) =
    expectedResults
    |> List.mapi (fun depthMinusOne expectedPerft -> testPerft name position (depthMinusOne + 1) expectedPerft)
    |> testList name

let mainPerftTests =
    perftTestData |> List.map testPerftList |> testList "Perft"

let testSlowPerfList ((name, position), expectedResults) =
    expectedResults
    |> List.map (fun (depth, expectedPerft) -> testPerft name position depth expectedPerft)
    |> testList name

let slowPerftTests =
    slowPerfTestData |> List.map testSlowPerfList |> ptestList "Slow perft" // usually set to pending

let debugTest =
    ptest "Debug" {
        let position = fromFen "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        let moves = allLegalMoves position

        moves |> List.iter (fun m -> printfn "%s %s %s" (m.From.toAlgebraic()) (m.To.toAlgebraic()) (toFen(makeMoveUnchecked position m)))

        let result = perft 1 position
        Expect.equal result 48 "Position should have 28 moves"
    }

let tests =
    testList "Perft tests" [
        mainPerftTests
        slowPerftTests
        debugTest
    ]
