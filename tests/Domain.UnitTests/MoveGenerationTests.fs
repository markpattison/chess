module Chess.Domain.UnitTests.MoveGenerationTests

open Expecto

open Chess.Domain.Position
open Chess.Domain.Move
open Chess.Domain.MoveGeneration

let emptyPosition =
    {
        Board = Board.empty()
        ToMove = White
        WhiteCastlingRights = CastlingRights.neither
        BlackCastlingRights = CastlingRights.neither
        EnPassantTargetSquare = None
        HalfMoveClock = 0
        FullMoveCount = 0
    }

let positionWithLonePiece square pieceSide =
    { emptyPosition with Board = Board.empty().Set(square, Some pieceSide) }

let corners =
    [
        { Row = 0; Col = 0 }
        { Row = 0; Col = 7 }
        { Row = 7; Col = 7 }
        { Row = 7; Col = 0 }
    ]

let midEdges =
    [
        { Row = 0; Col = 3 }
        { Row = 4; Col = 7 }
        { Row = 7; Col = 4 }
        { Row = 7; Col = 3 }
    ]

let testKingInCorner square =
    test (sprintf "King on %O" square) {
        let position = positionWithLonePiece square (King, White)

        let moves = potentialKingMoves square position

        Expect.hasLength moves 3 "King in corner should have 3 moves"        
    }

let testKingOnEdge square =
    test (sprintf "King on %O" square) {
        let position = positionWithLonePiece square (King, White)

        let moves = potentialKingMoves square position

        Expect.hasLength moves 5 "King mid-edge should have 5 moves"        
    }

let kingTests =
    testList "King" [
        test "Middle" {
            let square = { Row = 3; Col = 3 }
            let position = positionWithLonePiece square (King, White)

            let moves = potentialKingMoves square position

            Expect.hasLength moves 8 "Lone king should have 8 moves"
        }

        corners |> List.map testKingInCorner |> testList "Corner"
        midEdges |> List.map testKingOnEdge |> testList "Mid-edge"
    ]

let testKnightInCorner square =
    test (sprintf "Knight on %O" square) {
        let position = positionWithLonePiece square (Knight, White)

        let moves = potentialKnightMoves square position

        Expect.hasLength moves 2 "Knight in corner should have 2 moves"        
    }

let testKnightOnEdge square =
    test (sprintf "Knight on %O" square) {
        let position = positionWithLonePiece square (Knight, White)

        let moves = potentialKnightMoves square position

        Expect.hasLength moves 4 "Knight mid-edge should have 4 moves"        
    }

let knightTests =
    testList "Knight" [
        test "Middle" {
            let square = { Row = 3; Col = 3 }
            let position = positionWithLonePiece square (Knight, White)

            let moves = potentialKnightMoves square position

            Expect.hasLength moves 8 "Lone knight should have 8 moves"
        }

        corners |> List.map testKnightInCorner |> testList "Corner"
        midEdges |> List.map testKnightOnEdge |> testList "Mid-edge"
    ]

let tests =
    testList "Move generation tests" [
        kingTests
        knightTests
    ]