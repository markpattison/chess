module Chess.Domain.UnitTests.BoardTests

open Expecto

open Chess.Domain

let toFenTestInitial =
    test "FEN of initial position" {
        let initialPosition = getInitialPosition()
        let expected = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        Expect.equal (toFen initialPosition) expected "FEN should be correct"
    }

let toFenTestSicialian =
    test "FEN of position after 1. e4 e5" {
        let (Board board) = getInitialBoard()
        board.[1, 4] <- None
        board.[3, 4] <- Some (Pawn, White)
        board.[6, 2] <- None
        board.[4, 2] <- Some (Pawn, Black)

        let position = {
            Board = Board board
            ToMove = White
            WhiteCastlingRights = CastlingRights.both
            BlackCastlingRights = CastlingRights.both
            EnPassantTargetSquare = Some (Square.fromAlgebraic("c6"))
            HalfMoveClock = 0
            FullMoveCount = 2
        }

        let expected = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

        Expect.equal (toFen position) expected "FEN should be correct"
    }

let toFenTestSicialianNf3 =
    test "FEN of position after 1. e4 e5 2. Nf3" {
        let (Board board) = getInitialBoard()
        board.[1, 4] <- None
        board.[3, 4] <- Some (Pawn, White)
        board.[6, 2] <- None
        board.[4, 2] <- Some (Pawn, Black)
        board.[0, 6] <- None
        board.[2, 5] <- Some (Knight, White)

        let position = {
            Board = Board board
            ToMove = Black
            WhiteCastlingRights = CastlingRights.both
            BlackCastlingRights = CastlingRights.both
            EnPassantTargetSquare = None
            HalfMoveClock = 1
            FullMoveCount = 2
        }

        let expected = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

        Expect.equal (toFen position) expected "FEN should be correct"
    }

let fromFenTestInitial =
    test "Initial position from FEN" {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        let expected = getInitialPosition()

        Expect.equal (fromFen fen) expected "Position should be correct"
    }

let printTest =
    test "Print of initial board" {
        let initialPosition = getInitialPosition()
        let expected = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR\n"
        Expect.equal (print initialPosition.Board) expected "Print should be correct"
    }

let tests =
    testList "Board tests" [
        toFenTestInitial
        toFenTestSicialian
        toFenTestSicialianNf3
        fromFenTestInitial
        printTest
    ]