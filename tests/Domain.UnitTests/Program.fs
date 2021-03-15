module Chess.Domain.UnitTests.Program

open Expecto

[<EntryPoint>]
let main args =

    let allTests =
        testList "Unit tests" [
            SquareTests.tests
            PositionTests.tests
            FenTests.tests
            MoveGenerationTests.tests
            PerftTests.tests
        ]

    runTestsWithCLIArgs [] args allTests
