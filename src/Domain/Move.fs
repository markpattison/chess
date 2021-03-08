module Chess.Domain.Move

open Position

type PromotionType =
    | PromotionQueen
    | PromotionRook
    | PromotionBishop
    | PromotionKnight
    member this.toPiece() =
        match this with
        | PromotionQueen -> Queen
        | PromotionRook -> Rook
        | PromotionBishop -> Bishop
        | PromotionKnight -> Knight

type MoveType =
    | Quiet
    | CastleKingSide
    | CastleQueenSide
    | DoublePawnPush
    | EnPassantCapture
    | Capture of Piece
    | Promotion of PromotionType
    | CapturePromotion of Piece * PromotionType

type Move =
    {
        From: Square
        To: Square
        Type: MoveType
    }

