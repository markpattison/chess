module Chess.Domain.UnitTests.SquareTests

open Expecto

open Chess.Domain.Position

let sampleSquares = [
    "e2", 1, 4
    "a1", 0, 0
    "h8", 7, 7 ]

let sampleInvalidAlgebraic = [
    "h", "length 1"
    "h10", "length 3"
    "i2", "column i"
    "22", "column 2"
    "b0", "row 0"
    "b9", "row 9"
    "cc", "row c" ]

let testRowColToAlgebraic (alg, row, col) =
    test $"Row/col to algebraic: {alg}" {
        let square = { Row = row; Col = col }
        Expect.equal (square.toAlgebraic()) alg "Algebraic notation should be correct"
    }

let testAlgebraicToRowCol (alg, row, col) =
    test $"Algebraic to row/col: {alg}" {
        Expect.equal (Square.fromAlgebraic alg) { Row = row; Col = col } "Row/column should be correct"
    }

let testInvalidAlgebraic (alg, name) =
    test $"Invalid algebraic notation: {name}" {
        Expect.throws (fun _ -> Square.fromAlgebraic alg |> ignore) "Invalid algebraic notation should throw an exception"
    }

let tests =
    testList "Square tests" [
        sampleSquares |> List.map testRowColToAlgebraic |> testList "Row/col to algebraic"
        sampleSquares |> List.map testAlgebraicToRowCol |> testList "Algebraic to row/col"
        sampleInvalidAlgebraic |> List.map testInvalidAlgebraic |> testList "Invalid algebraic"
    ]