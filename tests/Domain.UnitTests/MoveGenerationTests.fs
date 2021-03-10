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

let positionWithPieces pieceSideSquares =
    let board = Board.empty()
    pieceSideSquares |> Seq.iter (fun (piece, side, square) -> board.Set(square, Some (piece, side)) |> ignore)
    { emptyPosition with Board = board }

let blackToMove position = { position with ToMove = Black }
let withEnPassantTarget square position = { position with EnPassantTargetSquare = Some square }

let corners = [ a1; a8; h1; h8 ]
let midEdges = [ d1; h5; e8; a4 ]

let promotionTypes = [ PromotionKnight; PromotionBishop; PromotionRook; PromotionQueen ]

let pawnTests =
    testList "Pawn" [
        test "White pawn on second rank" {
            let square = c2
            let position = positionWithLonePiece square (Pawn, White)

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 2 "Lone pawn on second rank should have 2 moves"
        }

        test "Black pawn on second rank" {
            let square = d7
            let position = positionWithLonePiece square (Pawn, Black) |> blackToMove

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 2 "Lone pawn on second rank should have 2 moves"
        }

        test "White pawn on third rank" {
            let square = c3
            let position = positionWithLonePiece square (Pawn, White)

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 1 "Lone pawn on third rank should have 1 move"
        }

        test "Black pawn on third rank" {
            let square = d6
            let position = positionWithLonePiece square (Pawn, Black) |> blackToMove

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 1 "Lone pawn on third rank should have 1 move"
        }

        test "White pawn on seventh rank" {
            let square = c7
            let position = positionWithLonePiece square (Pawn, White)

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 4 "Lone pawn on seventh rank should have 4 moves"
            Expect.all moves (fun m -> m.IsPromotion()) "All pawn moves from seventh rank should be promotions"
            promotionTypes |> List.iter (fun pt -> Expect.exists moves (fun m -> m.Type.PromotesTo() = Some pt) "All promotion options should be possible")
        }

        test "Black pawn on seventh rank" {
            let square = d2
            let position = positionWithLonePiece square (Pawn, Black) |> blackToMove

            let moves = potentialPawnMoves square position
            
            Expect.hasLength moves 4 "Lone pawn on seventh rank should have 4 moves"
            Expect.all moves (fun m -> m.IsPromotion()) "All pawn moves from seventh rank should be promotions"
            promotionTypes |> List.iter (fun pt -> Expect.exists moves (fun m -> m.Type.PromotesTo() = Some pt) "All promotion options should be possible")
        }

        test "White pawn with available captures" {
            let square = c3
            let position = positionWithPieces [ Pawn, White, c3; Pawn, Black, b4; Pawn, Black, d4 ]
            
            let moves = potentialPawnMoves square position

            moves |> Seq.iter (fun m -> printfn "From: %O to: %O type: %O" m.From m.To m.Type)

            Expect.hasLength moves 3 "Pawn with available captures should have 3 moves"
            Expect.exists moves (fun m -> m.To = b4) "Pawn can capture on b4"
            Expect.exists moves (fun m -> m.To = d4) "Pawn can capture on d4"
        }

        test "Black pawn with available captures" {
            let square = d6
            let position = positionWithPieces [ Pawn, Black, d6; Pawn, White, c5; Pawn, White, e5 ] |> blackToMove

            let moves = potentialPawnMoves square position

            Expect.hasLength moves 3 "Pawn with available captures should have 3 moves"
            Expect.exists moves (fun m -> m.To = c5) "Pawn can capture on c5"
            Expect.exists moves (fun m -> m.To = e5) "Pawn can capture on e5"
        }

        test "White pawn on seventh rank with available captures" {
            let square = g7
            let position = positionWithPieces [ Pawn, White, g7; Rook, Black, f8; Rook, Black, h8 ]
            
            let moves = potentialPawnMoves square position

            moves |> Seq.iter (fun m -> printfn "From: %O to: %O type: %O" m.From m.To m.Type)

            Expect.hasLength moves 12 "Pawn on seventh rank with available captures should have 12 moves"
            Expect.all moves (fun m -> m.IsPromotion()) "All pawn moves from seventh rank should be promotions"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = f8)) 4 "Four capture-promotions to a1"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = g8)) 4 "Four promotions to b1"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = h8)) 4 "Four capture-promotions to c1"
            promotionTypes |> List.iter (fun pt -> Expect.hasLength (moves |> List.filter (fun m -> m.Type.PromotesTo() = Some pt)) 3 "Three moves for each promotion type")
        }

        test "Black pawn on seventh rank with available captures" {
            let square = b2
            let position = positionWithPieces [ Pawn, Black, b2; Knight, White, a1; Queen, White, c1 ] |> blackToMove

            let moves = potentialPawnMoves square position

            Expect.hasLength moves 12 "Pawn on seventh rank with available captures should have 12 moves"
            Expect.all moves (fun m -> m.IsPromotion()) "All pawn moves from seventh rank should be promotions"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = a1)) 4 "Four capture-promotions to a1"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = b1)) 4 "Four promotions to b1"
            Expect.hasLength (moves |> List.filter (fun m -> m.To = c1)) 4 "Four capture-promotions to c1"
            promotionTypes |> List.iter (fun pt -> Expect.hasLength (moves |> List.filter (fun m -> m.Type.PromotesTo() = Some pt)) 3 "Three moves for each promotion type")
        }

        test "White pawn with en passant capture" {
            let square = c5
            let position = positionWithPieces [ Pawn, White, c5; Pawn, Black, d5 ] |> withEnPassantTarget d6

            let moves = potentialPawnMoves square position

            Expect.hasLength moves 2 "Pawn should have two moves"
            Expect.exists moves (fun m -> m.Type = EnPassantCapture) "Pawn can capture en passant"
        }

        test "Black pawn with en passant capture" {
            let square = d4
            let position = positionWithPieces [ Pawn, Black, d4; Pawn, White, c4 ] |> withEnPassantTarget c3 |> blackToMove

            let moves = potentialPawnMoves square position

            Expect.hasLength moves 2 "Pawn should have two moves"
            Expect.exists moves (fun m -> m.Type = EnPassantCapture) "Pawn can capture en passant"
        }
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
    testList "Potential move generation tests" [
        pawnTests
        kingTests
        knightTests
        bishopTests
        rookTests
        queenTests
    ]