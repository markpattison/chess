module Chess.Domain.Fen

open System

open Position

// see https://www.chessprogramming.org/Forsyth-Edwards_Notation

type private PieceFromCharResult =
    | PieceSide of Piece * Side
    | Empty of int

let private pieceChar (pieceSide: Piece * Side) =
    match pieceSide with
    | piece, White -> piece.ToString()
    | piece, Black -> piece.ToString().ToLower()

let rec private pieceFolder accString accBlanks remaining =
    match remaining, accBlanks with
    | [], 0 -> accString
    | [], n -> accString + n.ToString()
    | None :: xs, n -> pieceFolder accString (n + 1) xs
    | Some pieceSide :: xs, 0 -> pieceFolder (accString + pieceChar pieceSide) 0 xs
    | Some pieceSide :: xs, n -> pieceFolder (accString + n.ToString() + pieceChar pieceSide) 0 xs

let private toRow = pieceFolder "" 0

let private toPieces (Board board) =
    let piecesByRow =
        [| 7 .. -1 .. 0 |]
        |> Array.map (fun row ->
            board.[row, *] |> Array.toList |> toRow)

    String.Join('/', piecesByRow)    

let private toCastlingAbility position =
    if not (position.WhiteCastlingRights.KingSide || position.WhiteCastlingRights.QueenSide || position.BlackCastlingRights.KingSide || position.BlackCastlingRights.QueenSide) then
        "-"
    else
        (if position.WhiteCastlingRights.KingSide then "K" else "") +
        (if position.WhiteCastlingRights.KingSide then "Q" else "") +
        (if position.BlackCastlingRights.KingSide then "k" else "") +
        (if position.BlackCastlingRights.KingSide then "q" else "")

let private toEnPassantTargetSquare (squareOpt: Square option) =
    match squareOpt with
    | None -> "-"
    | Some square -> square.toAlgebraic()

let toFen position =
    sprintf "%s %s %s %s %i %i"
        (toPieces position.Board)
        (position.ToMove.ToLower())
        (toCastlingAbility position)
        (toEnPassantTargetSquare position.EnPassantTargetSquare)
        position.HalfMoveClock
        position.FullMoveCount

let private pieceFromChar c =
    match c with
    | 'P' -> PieceSide (Pawn, White)
    | 'N' -> PieceSide (Knight, White)
    | 'B' -> PieceSide (Bishop, White)
    | 'R' -> PieceSide (Rook, White)
    | 'Q' -> PieceSide (Queen, White)
    | 'K' -> PieceSide (King, White)
    | 'p' -> PieceSide (Pawn, Black)
    | 'n' -> PieceSide (Knight, Black)
    | 'b' -> PieceSide (Bishop, Black)
    | 'r' -> PieceSide (Rook, Black)
    | 'q' -> PieceSide (Queen, Black)
    | 'k' -> PieceSide (King, Black)
    | other ->
        match Int32.TryParse (other.ToString()) with
        | true, n -> Empty n
        | false, _ -> failwithf "Invalid piece in FEN: %c" other

let private fromPieces (pieces: string) =
    let rows = pieces.Split("/")

    if rows.Length <> 8 then failwithf "Invalid number of rows in FEN: %s" pieces

    let (Board board) = Board.empty()

    rows |> Array.iteri (fun reverseRow rowPieces ->
        let mutable nextCol = 0

        rowPieces |> Seq.iter (fun c ->
            match pieceFromChar c with
            | PieceSide (piece, side) ->
                if nextCol < 8 then board.[7 - reverseRow, nextCol] <- Some (piece, side)
                nextCol <- nextCol + 1
            | Empty n ->
                nextCol <- nextCol + n
        )

        if nextCol <> 8 then failwithf "Invalid length of row in FEN: %s" rowPieces
    )

    Board board

let private fromToMove sideToMove =
    match sideToMove with | "w" -> White | "b" -> Black | _ -> failwithf "Invalid side to move in FEN: %s" sideToMove

let private fromCastlingRightsWhite (castlingRights: string) =
    {
        KingSide = castlingRights.Contains("K")
        QueenSide = castlingRights.Contains("Q")
    }

let private fromCastlingRightsBlack (castlingRights: string) =
    {
        KingSide = castlingRights.Contains("k")
        QueenSide = castlingRights.Contains("q")
    }

let private fromEnPassantTargetSquare enPassantTargetSquare =
    match enPassantTargetSquare with
    | "-" -> None
    | alg -> Square.fromAlgebraic(alg) |> Some

let private fromHalfMoves (halfMoveClock: string) =
    match Int32.TryParse halfMoveClock with
    | true, hm -> hm
    | false, _ -> failwithf "Invalid half move clock in FEN: %s" halfMoveClock

let private fromFullMoveCount (fullMoveCount: string) =
    match Int32.TryParse fullMoveCount with
    | true, fm -> fm
    | false, _ -> failwithf "Invalid full move count in FEN: %s" fullMoveCount

let fromFen (fen: string) =

    let parts = fen.Trim().Split(" ") |> Array.toList

    match parts with
    | [ pieces; sideToMove; castlingRights; enPassantTargetSquare; halfMoveClock; fullMoveCount ] ->
        {
            Board = fromPieces pieces
            ToMove = fromToMove sideToMove
            WhiteCastlingRights = fromCastlingRightsWhite castlingRights
            BlackCastlingRights = fromCastlingRightsBlack castlingRights
            EnPassantTargetSquare = fromEnPassantTargetSquare enPassantTargetSquare
            HalfMoveClock = fromHalfMoves halfMoveClock
            FullMoveCount = fromFullMoveCount fullMoveCount
        }        

    | _ -> failwithf "FEN must have six fields: %s" fen
