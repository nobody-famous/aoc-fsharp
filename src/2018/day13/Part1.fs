module y2018.day13.part1

type Track = Horizontal | Vertical | CurveRight | CurveLeft

type Cart = Up | Down | Right | Left

type Piece = Track | Cart

let run (input:string) =
    printfn $"Part 1: {input}"

    ""
