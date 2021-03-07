module Chess.Domain.Position

open System

type Side =
    | White
    | Black
    member this.Opposite() = match this with | White -> Black | Black -> White
    override this.ToString() = match this with | White -> "White" | Black -> "Black"
    member this.ToUpper() = match this with | White -> "W" | Black -> "B"
    member this.ToLower() = match this with | White -> "w" | Black -> "b"

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

let print (Board board) =
    let sb = Text.StringBuilder()
    for row in 7 .. -1 .. 0 do
        for col in 0 .. 7 do
            sb.Append(sprintf "%s" (pieceOptChar board.[row, col])) |> ignore
        sb.Append("\n") |> ignore
    
    sb.ToString()
