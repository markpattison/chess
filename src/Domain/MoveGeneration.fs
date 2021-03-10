module Chess.Domain.MoveGeneration

open Position
open Move

let captureOrOwnPiece position fromSquare toSquare =
    let (Board board) = position.Board
    let otherSide = position.ToMove.Opposite()

    match board.[toSquare.Row, toSquare.Col] with
    | None -> Some { From = fromSquare; To = toSquare; Type = Quiet }
    | Some (piece, side) when side = otherSide -> Some { From = fromSquare; To = toSquare; Type = Capture piece }
    | _ -> None

let potentialKingMoves square position =
    let row, col = square.Row, square.Col

    let potentialSquares =
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
    
    potentialSquares |> List.choose (captureOrOwnPiece position square)

let potentialKnightMoves square position =
    let row, col = square.Row, square.Col

    let potentialSquares =
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
    
    potentialSquares |> List.choose (captureOrOwnPiece position square)

let private bishopDirections = [ (1, 1); (1, -1); (-1, -1); (-1, 1) ]
let private rookDirections = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
let private queenDirections = bishopDirections @ rookDirections

let (|OnBoard|Outside|) square =
    if square.Row >= 0 && square.Row <= 7 && square.Col >= 0 && square.Col <= 7 then OnBoard else Outside

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
