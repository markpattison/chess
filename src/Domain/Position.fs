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
    override this.ToString() = this.toAlgebraic()
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

let a1 = { Row = 0; Col = 0 }
let a2 = { Row = 1; Col = 0 }
let a3 = { Row = 2; Col = 0 }
let a4 = { Row = 3; Col = 0 }
let a5 = { Row = 4; Col = 0 }
let a6 = { Row = 5; Col = 0 }
let a7 = { Row = 6; Col = 0 }
let a8 = { Row = 7; Col = 0 }
let b1 = { Row = 0; Col = 1 }
let b2 = { Row = 1; Col = 1 }
let b3 = { Row = 2; Col = 1 }
let b4 = { Row = 3; Col = 1 }
let b5 = { Row = 4; Col = 1 }
let b6 = { Row = 5; Col = 1 }
let b7 = { Row = 6; Col = 1 }
let b8 = { Row = 7; Col = 1 }
let c1 = { Row = 0; Col = 2 }
let c2 = { Row = 1; Col = 2 }
let c3 = { Row = 2; Col = 2 }
let c4 = { Row = 3; Col = 2 }
let c5 = { Row = 4; Col = 2 }
let c6 = { Row = 5; Col = 2 }
let c7 = { Row = 6; Col = 2 }
let c8 = { Row = 7; Col = 2 }
let d1 = { Row = 0; Col = 3 }
let d2 = { Row = 1; Col = 3 }
let d3 = { Row = 2; Col = 3 }
let d4 = { Row = 3; Col = 3 }
let d5 = { Row = 4; Col = 3 }
let d6 = { Row = 5; Col = 3 }
let d7 = { Row = 6; Col = 3 }
let d8 = { Row = 7; Col = 3 }
let e1 = { Row = 0; Col = 4 }
let e2 = { Row = 1; Col = 4 }
let e3 = { Row = 2; Col = 4 }
let e4 = { Row = 3; Col = 4 }
let e5 = { Row = 4; Col = 4 }
let e6 = { Row = 5; Col = 4 }
let e7 = { Row = 6; Col = 4 }
let e8 = { Row = 7; Col = 4 }
let f1 = { Row = 0; Col = 5 }
let f2 = { Row = 1; Col = 5 }
let f3 = { Row = 2; Col = 5 }
let f4 = { Row = 3; Col = 5 }
let f5 = { Row = 4; Col = 5 }
let f6 = { Row = 5; Col = 5 }
let f7 = { Row = 6; Col = 5 }
let f8 = { Row = 7; Col = 5 }
let g1 = { Row = 0; Col = 6 }
let g2 = { Row = 1; Col = 6 }
let g3 = { Row = 2; Col = 6 }
let g4 = { Row = 3; Col = 6 }
let g5 = { Row = 4; Col = 6 }
let g6 = { Row = 5; Col = 6 }
let g7 = { Row = 6; Col = 6 }
let g8 = { Row = 7; Col = 6 }
let h1 = { Row = 0; Col = 7 }
let h2 = { Row = 1; Col = 7 }
let h3 = { Row = 2; Col = 7 }
let h4 = { Row = 3; Col = 7 }
let h5 = { Row = 4; Col = 7 }
let h6 = { Row = 5; Col = 7 }
let h7 = { Row = 6; Col = 7 }
let h8 = { Row = 7; Col = 7 }

type Board = | Board of ((Piece * Side) option [,]) with
    static member empty() : Board = Board (Array2D.create 8 8 None)
    member this.Get(square) = match this with | Board board -> board.[square.Row, square.Col]
    member this.Set(square, pieceSide) = match this with | Board board -> board.[square.Row, square.Col] <- pieceSide; this
    member this.Clone() = match this with | Board board -> Array2D.copy board |> Board

type CastlingRights =
    {
        KingSide: bool
        QueenSide: bool
    }
    static member both = { KingSide = true; QueenSide = true }
    static member neither = { KingSide = false; QueenSide = false }

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
