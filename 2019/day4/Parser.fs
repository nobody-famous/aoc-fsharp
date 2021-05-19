module aoc.year2019.day4.parser

let parseRange (line: string) =
    line.Split '-'
    |> Array.map
        (fun s ->
            s.ToCharArray()
            |> Array.map (fun ch -> int ch - int '0'))

let parseInput fileName =
    utils.parser.readLines fileName
    |> utils.parser.grabFirst
    |> parseRange
