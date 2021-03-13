module Chess.Domain.MoveGeneration

open Position
open Move

let private potentialKnightSquares square =
    let row, col = square.Row, square.Col

    [
        if row < 6 && col < 7 then { Row = row + 2; Col = col + 1 }
        if row < 7 && col < 6 then { Row = row + 1; Col = col + 2 }
        if row > 0 && col < 6 then { Row = row - 1; Col = col + 2 }
        if row > 1 && col < 7 then { Row = row - 2; Col = col + 1 }
        if row > 1 && col > 0 then { Row = row - 2; Col = col - 1 }
        if row > 0 && col > 1 then { Row = row - 1; Col = col - 2 }
        if row < 7 && col > 1 then { Row = row + 1; Col = col - 2 }
        if row < 6 && col > 0 then { Row = row + 2; Col = col - 1 }
    ]

let private potentialKingSquares square =
    let row, col = square.Row, square.Col

    [
        if row > 0            then { Row = row - 1; Col = col     }
        if row > 0 && col > 0 then { Row = row - 1; Col = col - 1 }
        if            col > 0 then { Row = row;     Col = col - 1 }
        if row < 7 && col > 0 then { Row = row + 1; Col = col - 1 }
        if row < 7            then { Row = row + 1; Col = col     }
        if row < 7 && col < 7 then { Row = row + 1; Col = col + 1 }
        if            col < 7 then { Row = row;     Col = col + 1 }
        if row > 0 && col < 7 then { Row = row - 1; Col = col + 1 }
    ]

let private bishopDirections = [ (1, 1); (1, -1); (-1, -1); (-1, 1) ]
let private rookDirections = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
let private queenDirections = bishopDirections @ rookDirections

let private (|OnBoard|Outside|) square =
    if square.Row >= 0 && square.Row <= 7 && square.Col >= 0 && square.Col <= 7 then OnBoard else Outside

let private captureOrOwnPiece position fromSquare toSquare =
    let (Board board) = position.Board
    let otherSide = position.ToMove.Opposite()

    match board.[toSquare.Row, toSquare.Col] with
    | None -> Some { From = fromSquare; To = toSquare; Type = Quiet }
    | Some (piece, side) when side = otherSide -> Some { From = fromSquare; To = toSquare; Type = Capture piece }
    | _ -> None

let private isAttackedByKnight position bySide square =
    potentialKnightSquares square |> List.exists (fun sq -> position.Board.Get(sq) = Some (Knight, bySide))

let private isAttackedByKing position bySide square =
    potentialKingSquares square |> List.exists (fun sq -> position.Board.Get(sq) = Some (King, bySide))

let private isAttackedByDiagonalPiece position bySide square (rowInc, colInc) =
    let rec isAttackedDiagonally sq =
        let nextSquare = { Row = sq.Row + rowInc; Col = sq.Col + colInc }
        match nextSquare with
        | Outside -> false
        | OnBoard ->
            match position.Board.Get(nextSquare) with
            | Some (piece, side) when side = bySide && (piece = Queen || piece = Bishop) -> true
            | Some _ -> false
            | None -> isAttackedDiagonally nextSquare
    
    isAttackedDiagonally square

let private isAttackedOnDiagonal position bySide square =
    bishopDirections |> List.exists (isAttackedByDiagonalPiece position bySide square)

let private isAttackedByRankAndFilePiece position bySide square (rowInc, colInc) =
    let rec isAttackedRankAndFile sq =
        let nextSquare = { Row = sq.Row + rowInc; Col = sq.Col + colInc }
        match nextSquare with
        | Outside -> false
        | OnBoard ->
            match position.Board.Get(nextSquare) with
            | Some (piece, side) when side = bySide && (piece = Queen || piece = Rook) -> true
            | Some _ -> false
            | None -> isAttackedRankAndFile nextSquare
    
    isAttackedRankAndFile square

let private isAttackedOnRankAndFile position bySide square =
    rookDirections |> List.exists (isAttackedByRankAndFilePiece position bySide square)

let private isAttackedByPawn position bySide square =
    let pawnRow = match bySide with | White -> square.Row - 1 | Black -> square.Row + 1

    (square.Col > 0 && pawnRow >= 0 && pawnRow <= 7 && position.Board.Get({ Row = pawnRow; Col = square.Col - 1}) = Some (Pawn, bySide))
    || (square.Col < 7 && pawnRow >= 0 && pawnRow <= 7 && position.Board.Get({ Row = pawnRow; Col = square.Col + 1}) = Some (Pawn, bySide))

let private isAttackedBy position bySide square =
    isAttackedByKnight position bySide square
    || isAttackedOnRankAndFile position bySide square
    || isAttackedOnDiagonal position bySide square
    || isAttackedByPawn position bySide square
    || isAttackedByKing position bySide square

let potentialKingMoves square position =
    let potentialSquares =
        potentialKingSquares square
        |> List.map (captureOrOwnPiece position square)

    let castleWhiteKingSide =
        if position.ToMove = White
            && square = e1
            && position.Board.Get(e1) = Some (King, White)
            && position.Board.Get(f1) = None
            && position.Board.Get(g1) = None
            && position.Board.Get(h1) = Some (Rook, White)
            && position.WhiteCastlingRights.KingSide
            && not (isAttackedBy position Black e1)
            && not (isAttackedBy position Black f1)
        then
            Some { From = e1; To = g1; Type = CastleKingSide } 
        else
            None
    
    let castleWhiteQueenSide =
        if position.ToMove = White
            && square = e1
            && position.Board.Get(e1) = Some (King, White)
            && position.Board.Get(d1) = None
            && position.Board.Get(c1) = None
            && position.Board.Get(b1) = None
            && position.Board.Get(a1) = Some (Rook, White)
            && position.WhiteCastlingRights.QueenSide
            && not (isAttackedBy position Black e1)
            && not (isAttackedBy position Black d1)
        then
            Some { From = e1; To = c1; Type = CastleQueenSide }
        else
            None

    let castleBlackKingSide =
        if position.ToMove = Black
            && square = e8
            && position.Board.Get(e8) = Some (King, Black)
            && position.Board.Get(f8) = None
            && position.Board.Get(g8) = None
            && position.Board.Get(h8) = Some (Rook, Black)
            && position.BlackCastlingRights.KingSide
            && not (isAttackedBy position White e8)
            && not (isAttackedBy position White f8)
        then
            Some { From = e8; To = g8; Type = CastleKingSide } 
        else
            None
    
    let castleBlackQueenSide =
        if position.ToMove = Black
            && square = e8
            && position.Board.Get(e8) = Some (King, Black)
            && position.Board.Get(d8) = None
            && position.Board.Get(c8) = None
            && position.Board.Get(b8) = None
            && position.Board.Get(a8) = Some (Rook, Black)
            && position.BlackCastlingRights.QueenSide
            && not (isAttackedBy position White e8)
            && not (isAttackedBy position White d8)
        then
            Some { From = e8; To = c8; Type = CastleQueenSide }
        else
            None
                
    [ castleWhiteKingSide; castleWhiteQueenSide; castleBlackKingSide; castleBlackQueenSide ] @ potentialSquares
    |> List.choose id
    
let potentialKnightMoves square position =
    potentialKnightSquares square |> List.choose (captureOrOwnPiece position square)

let private potentialMovesForDirection fromSquare position (rowInc, colInc) =
    let otherSide = position.ToMove.Opposite()
    let rec moves sq = seq {
        let nextSquare = { Row = sq.Row + rowInc; Col = sq.Col + colInc }
        match nextSquare with
        | Outside -> ()
        | OnBoard ->
            match position.Board.Get(nextSquare) with
            | Some (piece, side) when side = otherSide -> yield { From = fromSquare; To = nextSquare; Type = Capture piece }
            | Some _ -> ()
            | None ->
                yield { From = fromSquare; To = nextSquare; Type = Quiet }
                yield! moves nextSquare
    }
    
    moves fromSquare

let private potentialMoves directions square position =
    directions
    |> Seq.collect (potentialMovesForDirection square position)
    |> Seq.toList

let potentialBishopMoves = potentialMoves bishopDirections
let potentialRookMoves = potentialMoves rookDirections
let potentialQueenMoves = potentialMoves queenDirections

let private promotionOptions = [ PromotionKnight; PromotionBishop; PromotionRook; PromotionQueen ]

let private allPromotions fromSquare toSquare =
    promotionOptions
    |> List.map (fun p -> { From = fromSquare; To = toSquare; Type = Promotion p })

let private allCapturePromotions fromSquare toSquare captured =
    promotionOptions
    |> List.map (fun p -> { From = fromSquare; To = toSquare; Type = CapturePromotion (captured, p) })

let private potentialWhitePawnMoves square position =
    let otherSide = position.ToMove.Opposite()
    let row, col = square.Row, square.Col
    let nextSquare = { Row = row + 1; Col = col }
    let nextSquare2 = { Row = row + 2; Col = col }
    let leftCapture = { Row = row + 1; Col = col - 1 }
    let rightCapture = { Row = row + 1; Col = col + 1 }
    
    [
        // move one square forward
        if row < 6 && position.Board.Get(nextSquare) = None then { From = square; To = nextSquare; Type = Quiet }

        // move two square forward
        if row = 1 && position.Board.Get(nextSquare) = None && position.Board.Get(nextSquare2) = None then { From = square; To = nextSquare2; Type = DoublePawnPush }

        // capture left
        if row < 6 && col > 0 then
            match position.Board.Get(leftCapture) with
            | Some (piece, side) when side = otherSide -> { From = square; To = leftCapture; Type = Capture piece }
            | _ -> ()
        
        // capture right
        if row < 6 && col < 7 then
            match position.Board.Get(rightCapture) with
            | Some (piece, side) when side = otherSide -> { From = square; To = rightCapture; Type = Capture piece }
            | _ -> ()
        
        // promote without capturing
        if row = 6 && position.Board.Get(nextSquare) = None then yield! allPromotions square nextSquare

        // promote while capturing left
        if row = 6 && col > 0 then
            match position.Board.Get(leftCapture) with
            | Some (piece, side) when side = otherSide -> yield! allCapturePromotions square leftCapture piece
            | _ -> ()
        
        // promote while capturing right
        if row = 6 && col < 7 then
            match position.Board.Get(rightCapture) with
            | Some (piece, side) when side = otherSide -> yield! allCapturePromotions square rightCapture piece
            | _ -> ()
        
        // en passant capture
        match position.EnPassantTargetSquare with
            | None -> ()
            | Some ep ->
                if row = 4 && (ep.Col = col - 1 || ep.Col = col + 1) then { From = square; To = ep; Type = EnPassantCapture }
    ]

let private potentialBlackPawnMoves square position =
    let otherSide = position.ToMove.Opposite()
    let row, col = square.Row, square.Col
    let nextSquare = { Row = row - 1; Col = col }
    let nextSquare2 = { Row = row - 2; Col = col }
    let leftCapture = { Row = row - 1; Col = col + 1 }
    let rightCapture = { Row = row - 1; Col = col - 1 }
    
    [
        // move one square forward
        if row > 1 && position.Board.Get(nextSquare) = None then { From = square; To = nextSquare; Type = Quiet }

        // move two square forward
        if row = 6 && position.Board.Get(nextSquare) = None && position.Board.Get(nextSquare2) = None then { From = square; To = nextSquare2; Type = DoublePawnPush }

        // capture left
        if row > 1 && col < 7 then
            match position.Board.Get(leftCapture) with
            | Some (piece, side) when side = otherSide -> { From = square; To = leftCapture; Type = Capture piece }
            | _ -> ()
        
        // capture right
        if row > 1 && col > 0 then
            match position.Board.Get(rightCapture) with
            | Some (piece, side) when side = otherSide -> { From = square; To = rightCapture; Type = Capture piece }
            | _ -> ()
        
        // promote without capturing
        if row = 1 && position.Board.Get(nextSquare) = None then yield! allPromotions square nextSquare

        // promote while capturing left
        if row = 1 && col < 7 then
            match position.Board.Get(leftCapture) with
            | Some (piece, side) when side = otherSide -> yield! allCapturePromotions square leftCapture piece
            | _ -> ()
        
        // promote while capturing right
        if row = 1 && col > 0 then
            match position.Board.Get(rightCapture) with
            | Some (piece, side) when side = otherSide -> yield! allCapturePromotions square rightCapture piece
            | _ -> ()
        
        // en passant capture
        match position.EnPassantTargetSquare with
            | None -> ()
            | Some ep ->
                if row = 3 && (ep.Col = col - 1 || ep.Col = col + 1) then { From = square; To = ep; Type = EnPassantCapture }
    ]

let potentialPawnMoves square position =
    match position.ToMove with
    | White -> potentialWhitePawnMoves square position
    | Black -> potentialBlackPawnMoves square position

let makeMoveUnchecked position move =
    let pieceSideOpt = position.Board.Get(move.From)
    let isPawnMove = match pieceSideOpt with Some (Pawn, _) -> true | _ -> false

    let halfMoveClock = if move.IsCapture() || isPawnMove then 0 else position.HalfMoveClock + 1
    let fullMoveCount = match position.ToMove with | White -> position.FullMoveCount | Black -> position.FullMoveCount + 1
    let enPassantTargetSquare =
        match move.Type with
        | DoublePawnPush -> Some ({ Row = (move.From.Row + move.To.Row) / 2; Col = move.From.Col })
        | _ -> None

    let whiteCastlingRights =
        match position.ToMove with
        | Black -> position.WhiteCastlingRights
        | White ->
            match pieceSideOpt, move.From with
            | Some (King, _), _ -> CastlingRights.neither
            | Some (Rook, _), sq when sq = h1 -> { position.WhiteCastlingRights with KingSide = false }
            | Some (Rook, _), sq when sq = a1 -> { position.WhiteCastlingRights with QueenSide = false }
            | _ -> position.WhiteCastlingRights
    
    let blackCastlingRights =
        match position.ToMove with
        | White -> position.BlackCastlingRights
        | Black ->
            match pieceSideOpt, move.From with
            | Some (King, _), _ -> CastlingRights.neither
            | Some (Rook, _), sq when sq = h8 -> { position.BlackCastlingRights with KingSide = false }
            | Some (Rook, _), sq when sq = a8 -> { position.BlackCastlingRights with QueenSide = false }
            | _ -> position.BlackCastlingRights
    
    let clone = position.Board.Clone()

    let updatedBoard =
        match move.Type with
        | CastleKingSide when position.ToMove = White -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set(h1, None).Set(f1, Some (Rook, White))
        | CastleKingSide when position.ToMove = Black -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set(h8, None).Set(f8, Some (Rook, Black))
        | CastleQueenSide when position.ToMove = White -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set(a1, None).Set(d1, Some (Rook, White))
        | CastleQueenSide when position.ToMove = Black -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set(a8, None).Set(d8, Some (Rook, Black))
        | EnPassantCapture when position.ToMove = White -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set({ Row = move.To.Row - 1; Col = move.To.Col }, None)
        | EnPassantCapture when position.ToMove = Black -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From)).Set({ Row = move.To.Row + 1; Col = move.To.Col }, None)
        | Promotion promoteTo | CapturePromotion (_, promoteTo) -> clone.Set(move.From, None).Set(move.To, Some (promoteTo.toPiece(), position.ToMove))
        | _ -> clone.Set(move.From, None).Set(move.To, position.Board.Get(move.From))

    {
        Board = updatedBoard
        ToMove = position.ToMove.Opposite()
        WhiteCastlingRights = whiteCastlingRights
        BlackCastlingRights = blackCastlingRights
        EnPassantTargetSquare = enPassantTargetSquare
        HalfMoveClock = halfMoveClock
        FullMoveCount = fullMoveCount
    }

let isInCheck position side =
    let kingSquare = position.Board.KingPosition(side)
    isAttackedBy position (side.Opposite()) kingSquare

let isMoveLegal position move =
    let updatedPosition = makeMoveUnchecked position move
    not (isInCheck updatedPosition position.ToMove)

let allLegalMoves position =
    let (Board board) = position.Board

    let potentialMoves = [
        for row in 0 .. 7 do
            for col in 0 .. 7 do
                match board.[row, col] with
                | Some (piece, side) when side = position.ToMove ->
                    let square = { Row = row; Col = col }
                    match piece with
                    | Pawn -> yield! potentialPawnMoves square position
                    | Knight -> yield! potentialKnightMoves square position
                    | Bishop -> yield! potentialBishopMoves square position
                    | Rook -> yield! potentialRookMoves square position
                    | Queen -> yield! potentialQueenMoves square position
                    | King -> yield! potentialKingMoves square position
                | _ -> ()
    ]

    let legalMoves = potentialMoves |> List.filter (isMoveLegal position)

    legalMoves

let rec perft depth position =
    if depth <= 0 then
        1
    else
        position
        |> allLegalMoves
        |> List.map (makeMoveUnchecked position)
        |> List.sumBy (perft (depth - 1))

// same as perft but prints out divisions at first level
let perftdiv depth position =
    if depth <= 0 then
        1
    else
        let nextPositions =
            position
            |> allLegalMoves
            |> List.map (makeMoveUnchecked position)
        
        let nextPerfts =
            nextPositions
            |> List.map (fun p -> p, perft (depth - 1) p)
        
        nextPerfts |> List.iter (fun (p, perfSplit) -> printfn "%s %i" (Fen.toFen(p)) perfSplit)
        nextPerfts |> List.sumBy snd
