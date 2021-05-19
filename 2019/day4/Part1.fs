module aoc.year2019.day4.part1

let splitInput (input: char array array) = (input.[0], input.[1])

let rec updateRest (arr: char array) (ch, ndx) =
    if ndx < arr.Length then
        arr.[ndx] <- ch
        updateRest arr <| (ch, ndx + 1)

let findFirst (low: char array, high) =
    let rec init ndx =
        if low.[ndx] < low.[ndx - 1] then
            (low.[ndx - 1], ndx)
        else
            init <| ndx + 1

    init 1 |> updateRest low

    (low, high)

let run fileName =
    let (low, high) =
        parser.parseInput fileName
        |> splitInput
        |> findFirst

    Array.iter (fun ch -> printf "%c" ch) low
    printfn ""

    0
