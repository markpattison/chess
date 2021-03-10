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

            Expect.hasLength moves 8 "King in middle should have 8 moves"
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

            Expect.hasLength moves 8 "Knight in middle should have 8 moves"
        }

        corners |> List.map testKnightInCorner |> testList "Corner"
        midEdges |> List.map testKnightOnEdge |> testList "Mid-edge"
    ]

let testBishopInCorner square =
    test (sprintf "Bishop on %O" square) {
        let position = positionWithLonePiece square (Bishop, White)

        let moves = potentialBishopMoves square position

        Expect.hasLength moves 7 "Bishop in corner should have 7 moves"        
    }

let testBishopOnEdge square =
    test (sprintf "Bishop on %O" square) {
        let position = positionWithLonePiece square (Bishop, White)

        let moves = potentialBishopMoves square position

        Expect.hasLength moves 7 "Bishop mid-edge should have 7 moves"        
    }

let bishopTests =
    testList "Bishop" [
        test "Middle" {
            let square = { Row = 3; Col = 3 }
            let position = positionWithLonePiece square (Bishop, White)

            let moves = potentialBishopMoves square position

            Expect.hasLength moves 13 "Bishop in middle should have 13 moves"
        }

        corners |> List.map testBishopInCorner |> testList "Corner"
        midEdges |> List.map testBishopOnEdge |> testList "Mid-edge"
    ]

let testRookInCorner square =
    test (sprintf "Rook on %O" square) {
        let position = positionWithLonePiece square (Rook, White)

        let moves = potentialRookMoves square position

        Expect.hasLength moves 14 "Rook in corner should have 14 moves"        
    }

let testRookOnEdge square =
    test (sprintf "Rook on %O" square) {
        let position = positionWithLonePiece square (Rook, White)

        let moves = potentialRookMoves square position

        Expect.hasLength moves 14 "Rook mid-edge should have 14 moves"        
    }

let rookTests =
    testList "Rook" [
        test "Middle" {
            let square = { Row = 3; Col = 3 }
            let position = positionWithLonePiece square (Rook, White)

            let moves = potentialRookMoves square position

            Expect.hasLength moves 14 "Rook in middle should have 14 moves"
        }

        corners |> List.map testRookInCorner |> testList "Corner"
        midEdges |> List.map testRookOnEdge |> testList "Mid-edge"
    ]

let testQueenInCorner square =
    test (sprintf "Queen on %O" square) {
        let position = positionWithLonePiece square (Queen, White)

        let moves = potentialQueenMoves square position

        Expect.hasLength moves 21 "Queen in corner should have 21 moves"        
    }

let testQueenOnEdge square =
    test (sprintf "Queen on %O" square) {
        let position = positionWithLonePiece square (Queen, White)

        let moves = potentialQueenMoves square position

        Expect.hasLength moves 21 "Queen mid-edge should have 21 moves"        
    }

let queenTests =
    testList "Queen" [
        test "Middle" {
            let square = { Row = 3; Col = 3 }
            let position = positionWithLonePiece square (Queen, White)

            let moves = potentialQueenMoves square position

            Expect.hasLength moves 27 "Queen in middle should have 27 moves"
        }

        corners |> List.map testQueenInCorner |> testList "Corner"
        midEdges |> List.map testQueenOnEdge |> testList "Mid-edge"
    ]

let tests =
    testList "Move generation tests" [
        kingTests
        knightTests
        bishopTests
        rookTests
        queenTests
    ]