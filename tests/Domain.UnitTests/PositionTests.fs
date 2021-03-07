module Chess.Domain.UnitTests.PositionTests

open Expecto

open Chess.Domain.Position

let tests =
    testList "Position tests" [
        test "Print of initial board" {
            let initialPosition = getInitialPosition()
            let expected = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR\n"
            Expect.equal (print initialPosition.Board) expected "Print should be correct"
        }
    ]