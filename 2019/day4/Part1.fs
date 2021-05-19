module aoc.year2019.day4.part1

let isValid (arr: int array) =
    let rec loop ndx =
        if ndx >= arr.Length then
            false
        else if arr.[ndx - 1] = arr.[ndx] then
            true
        else
            loop <| ndx + 1

    loop 1

let run fileName =
    parser.parseInput fileName
    |> utils.splitInput
    |> utils.findFirst
    |> utils.countValid isValid
