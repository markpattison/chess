module Chess.Domain.UnitTests.MoveGenerationTests

open Expecto

open Chess.Domain.Fen
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
let withCastlingRights side rights position =
    match side with
    | White -> { position with WhiteCastlingRights = rights }
    | Black -> { position with BlackCastlingRights = rights }

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

        test "White pawn cannot advance if blocked" {
            let square = h3
            let position = positionWithPieces [ Pawn, White, h3; Pawn, Black, h4 ]

            let moves = potentialPawnMoves square position
            Expect.isEmpty moves "Pawn should have no moves"
        }

        test "Black pawn cannot advance if blocked" {
            let square = a6
            let position = positionWithPieces [ Pawn, Black, a6; Pawn, White, a5 ] |> blackToMove

            let moves = potentialPawnMoves square position
            Expect.isEmpty moves "Pawn should have no moves"
        }

        test "White pawn on second rank cannot advance two squares if blocked on first square" {
            let square = a2
            let position = positionWithPieces [ Pawn, White, a2; Pawn, Black, a3 ]

            let moves = potentialPawnMoves square position
            Expect.isEmpty moves "Pawn should have no moves"
        }

        test "Black pawn on second rank cannot advance two squares if blocked on first square" {
            let square = h7
            let position = positionWithPieces [ Pawn, Black, h7; Pawn, White, h6 ] |> blackToMove

            let moves = potentialPawnMoves square position
            Expect.isEmpty moves "Pawn should have no moves"
        }
        test "White pawn on second rank cannot advance two squares if blocked on second square" {
            let square = a2
            let position = positionWithPieces [ Pawn, White, a2; Pawn, Black, a4 ]

            let moves = potentialPawnMoves square position
            Expect.hasLength moves 1 "Pawn should only have one move"
        }

        test "Black pawn on second rank cannot advance two squares if blocked on second square" {
            let square = h7
            let position = positionWithPieces [ Pawn, Black, h7; Pawn, White, h5 ] |> blackToMove

            let moves = potentialPawnMoves square position
            Expect.hasLength moves 1 "Pawn should only have one move"
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

        test "Can castle kingside" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.contains moves { From = e1; To = g1; Type = CastleKingSide } "King can castle kingside"
        }

        test "Cannot castle kingside with no castling rights" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleKingSide)) "Cannot castle kingside with no castling rights"
        }

        test "Cannot castle kingside when f1 occupied" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1; Bishop, White, f1 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleKingSide)) "Cannot castle kingside when f1 occupied"        
        }

        test "Cannot castle kingside when g1 occupied" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1; Knight, White, g1 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleKingSide)) "Cannot castle kingside when g1 occupied"        
        }

        test "Cannot castle kingside when in check" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1; Rook, Black, e8 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleKingSide)) "Cannot castle kingside when in check"
        }

        test "Cannot castle kingside through check" {
            let position = positionWithPieces [ King, White, e1; Rook, White, h1; Rook, Black, f8 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleKingSide)) "Cannot castle kingside through check"
        }

        test "Can castle queenside" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.contains moves { From = e1; To = c1; Type = CastleQueenSide } "King can castle queenside"
        }

        test "Cannot castle queenside with no castling rights" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1 ] |> withCastlingRights White { KingSide = true; QueenSide = false }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside with no castling rights"
        }

        test "Cannot castle queenside when d1 occupied" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1; Queen, White, d1 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside when d1 occupied"        
        }

        test "Cannot castle queenside when c1 occupied" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1; Bishop, White, c1 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside when c1 occupied"        
        }

        test "Cannot castle queenside when b1 occupied" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1; Knight, White, b1 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside when b1 occupied"        
        }

        test "Cannot castle queenside when in check" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1; Rook, Black, e8 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside when in check"
        }

        test "Cannot castle queenside through check" {
            let position = positionWithPieces [ King, White, e1; Rook, White, a1; Rook, Black, d8 ] |> withCastlingRights White { KingSide = false; QueenSide = true }

            let moves = potentialKingMoves e1 position

            Expect.isEmpty (moves |> List.filter (fun m -> m.Type = CastleQueenSide)) "Cannot castle queenside through check"
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

// see https://www.chessprogramming.org/Perft_Results

let testPositions =
    [
        "Initial position", getInitialPosition(), [ 20 ]
        "Position 2", fromFen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"), [ 48 ]
        "Position 3", fromFen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"), [ 14 ]
        "Position 4", fromFen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"), [ 6 ]
        "Position 4 mirrored", fromFen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"), [ 6 ]
        "Position 5", fromFen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"), [ 44 ]
        "Position 6", fromFen("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"), [ 46 ]
    ]

let testPerft name position depth expectedPerft =
    test name {
        let result = perft depth position

        Expect.equal result expectedPerft (sprintf "Perft depth %i for %s should be %i" depth name expectedPerft)
    }

let testAllPerft (name, position, expectedResults) =
    expectedResults
    |> List.mapi (fun depthMinusOne expectedPerft -> testPerft name position (depthMinusOne + 1) expectedPerft)
    |> testList (sprintf "Perft results for %s" name)

let allPerftTests =
    testPositions |> List.map testAllPerft |> testList "Correct perft results"

let tests =
    testList "Potential move generation tests" [
        pawnTests
        kingTests
        knightTests
        bishopTests
        rookTests
        queenTests
        allPerftTests
    ]