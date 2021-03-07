module Chess.Domain.Fen

open System

open Position

let toFen position =

    // see https://www.chessprogramming.org/Forsyth-Edwards_Notation

    let (Board board) = position.Board

    let rec pieceFolder accString accBlanks remaining =
        match remaining, accBlanks with
        | [], 0 -> accString
        | [], n -> accString + n.ToString()
        | None :: xs, n -> pieceFolder accString (n + 1) xs
        | Some pieceSide :: xs, 0 -> pieceFolder (accString + pieceChar pieceSide) 0 xs
        | Some pieceSide :: xs, n -> pieceFolder (accString + n.ToString() + pieceChar pieceSide) 0 xs

    let piecesByRow =
        [| 7 .. -1 .. 0 |]
        |> Array.map (fun row ->
            let rowPieces = board.[row, *] |> Array.toList
            pieceFolder "" 0 rowPieces)

    let pieces = String.Join('/', piecesByRow)

    let sideToMove = match position.ToMove with | White -> "w" | Black -> "b"

    let castlingAbility =
        if not (position.WhiteCastlingRights.KingSide || position.WhiteCastlingRights.QueenSide || position.BlackCastlingRights.KingSide || position.BlackCastlingRights.QueenSide) then
            "-"
        else
            (if position.WhiteCastlingRights.KingSide then "K" else "") +
            (if position.WhiteCastlingRights.KingSide then "Q" else "") +
            (if position.BlackCastlingRights.KingSide then "k" else "") +
            (if position.BlackCastlingRights.KingSide then "q" else "")

    let enPassantTargetSquare =
        match position.EnPassantTargetSquare with
        | None -> "-"
        | Some square -> square.toAlgebraic()

    sprintf "%s %s %s %s %i %i" pieces sideToMove castlingAbility enPassantTargetSquare position.HalfMoveClock position.FullMoveCount

type PieceFromCharResult =
    | PieceSide of Piece * Side
    | Empty of int

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

let private parseFen (pieces: string) sideToMove (castlingAbility: string) enPassantTargetSquare (halfMoveClock: string) (fullMoveCount: string) =

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

    let toMove = match sideToMove with | "w" -> White | "b" -> Black | _ -> failwithf "Invalid side to move in FEN: %s" sideToMove

    let whiteCastlingRights =
        {
            KingSide = castlingAbility.Contains("K")
            QueenSide = castlingAbility.Contains("Q")
        }

    let blackCastlingRights =
        {
            KingSide = castlingAbility.Contains("k")
            QueenSide = castlingAbility.Contains("q")
        }

    let enPassantTarget =
        match enPassantTargetSquare with
        | "-" -> None
        | alg -> Square.fromAlgebraic(alg) |> Some
    
    let halfMoves =
        match Int32.TryParse halfMoveClock with
        | true, hm -> hm
        | false, _ -> failwithf "Invalid half move clock in FEN: %s" halfMoveClock
    
    let fullMoves =
        match Int32.TryParse fullMoveCount with
        | true, fm -> fm
        | false, _ -> failwithf "Invalid full move count in FEN: %s" fullMoveCount
    
    {
        Board = Board board
        ToMove = toMove
        WhiteCastlingRights = whiteCastlingRights
        BlackCastlingRights = blackCastlingRights
        EnPassantTargetSquare = enPassantTarget
        HalfMoveClock = halfMoves
        FullMoveCount = fullMoves
    }

let fromFen (fen: string) =

    let parts = fen.Trim().Split(" ") |> Array.toList

    match parts with
    | [ pieces; sideToMove; castlingAbility; enPassantTargetSquare; halfMoveClock; fullMoveCount ] -> parseFen pieces sideToMove castlingAbility enPassantTargetSquare halfMoveClock fullMoveCount
    | _ -> failwithf "FEN must have six fields: %s" fen
