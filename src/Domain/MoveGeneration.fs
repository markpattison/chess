module Chess.Domain.MoveGeneration

open Position
open Move

let captureOrOwnPiece position square =
    let (Board board) = position.Board
    let otherSide = position.ToMove.Opposite()

    match board.[square.Row, square.Col] with
    | None -> Some Quiet
    | Some (piece, side) when side = otherSide -> Some (Capture piece)
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
    
    potentialSquares |> List.choose (captureOrOwnPiece position)

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
    
    potentialSquares |> List.choose (captureOrOwnPiece position)