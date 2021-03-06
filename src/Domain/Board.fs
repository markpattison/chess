module Chess.Domain

open System

type Side =
    | White
    | Black

type Piece =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
    override this.ToString() =
        match this with
        | Pawn -> "P"
        | Knight -> "N"
        | Bishop -> "B"
        | Rook -> "R"
        | Queen -> "Q"
        | King -> "K"

let pieceChar (pieceSide: Piece * Side) =
    match pieceSide with
    | piece, White -> piece.ToString()
    | piece, Black -> piece.ToString().ToLower()

let pieceOptChar (pieceOpt: (Piece * Side) option) =
    match pieceOpt with
    | None -> "."
    | Some (piece, White) -> piece.ToString()
    | Some (piece, Black) -> piece.ToString().ToLower()

type Square =
    {
        Row: int
        Col: int
    }
    member this.toAlgebraic() = sprintf "%c%i" (char (this.Col + 97)) (this.Row + 1)
    override this.ToString() = sprintf "%O (%i, %i)" this this.Row this.Col
    static member fromAlgebraic (alg: string) =
        match List.ofSeq alg with
        | [ algCol; algRow ] ->
            let col =
                match (int algCol - int 'a') with
                | c when c >=0 && c <= 7 -> c
                | _ -> failwithf "Algebraic notation column must be a-h: %s" alg
            let row =
                match (int algRow - int '1') with
                | r when r >= 0 && r <= 7 -> r
                | _ -> failwithf "Algebraic notation row must be 1-8: %s" alg
            { Row = row; Col = col }
        | _ -> failwithf "Algebraic notation must have two characters: %s" alg

type Board = | Board of ((Piece * Side) option [,])
    with static member empty() : Board = Board (Array2D.create 8 8 None)

type CastlingRights =
    {
        KingSide: bool
        QueenSide: bool
    }
    static member both = { KingSide = true; QueenSide = true }

type Position =
    {
        Board: Board
        ToMove: Side
        WhiteCastlingRights: CastlingRights
        BlackCastlingRights: CastlingRights
        EnPassantTargetSquare: Square option
        HalfMoveClock: int
        FullMoveCount: int
    }

let getInitialBoard() =
    let (Board board) = Board.empty()

    let backRank = [ Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook ]

    backRank |> List.iteri (fun col piece ->
        board.[0, col] <- Some (piece, White)
        board.[7, col] <- Some (piece, Black))
    
    [ 0 .. 7 ] |> List.iter (fun col ->
        board.[1, col] <- Some (Pawn, White)
        board.[6, col] <- Some (Pawn, Black))
    
    Board board

let getInitialPosition() =
    {
        Board = getInitialBoard()
        ToMove = White
        WhiteCastlingRights = CastlingRights.both
        BlackCastlingRights = CastlingRights.both
        EnPassantTargetSquare = None
        HalfMoveClock = 0
        FullMoveCount = 1
    }

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

let parseFen (pieces: string) sideToMove (castlingAbility: string) enPassantTargetSquare (halfMoveClock: string) (fullMoveCount: string) =

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

let print (Board board) =
    let sb = Text.StringBuilder()
    for row in 7 .. -1 .. 0 do
        for col in 0 .. 7 do
            sb.Append(sprintf "%s" (pieceOptChar board.[row, col])) |> ignore
        sb.Append("\n") |> ignore
    
    sb.ToString()
