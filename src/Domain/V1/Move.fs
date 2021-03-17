module Chess.Domain.V1.Move

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
    member this.Captures() =
        match this with
        | Capture p | CapturePromotion (p, _) -> Some p
        | EnPassantCapture -> Some Pawn
        | _ -> None
    member this.PromotesTo() =
        match this with
        | Promotion p | CapturePromotion (_, p) -> Some p
        | _ -> None

type Move =
    {
        From: Square
        To: Square
        Type: MoveType
    }
    member this.IsCapture() = this.Type.Captures().IsSome
    member this.IsPromotion() = this.Type.PromotesTo().IsSome
