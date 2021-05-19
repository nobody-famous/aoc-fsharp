module aoc.year2019.day3.parser

open utils.geometry

let parseDir (str: string) =
    let turn = str.[0]
    let dist = str.Substring 1 |> int

    match turn with
    | 'R' -> { x = dist; y = 0 }
    | 'L' -> { x = -dist; y = 0 }
    | 'U' -> { x = 0; y = dist }
    | 'D' -> { x = 0; y = -dist }
    | _ -> raise <| System.Exception $"Unhandled input {str}"

let toLines diffs =
    let (_, lines) =
        Array.fold
            (fun (prevPt, lines) pt ->
                let next =
                    { x = prevPt.x + pt.x
                      y = prevPt.y + pt.y }

                (next, { head = prevPt; tail = next } :: lines))
            ({ x = 0; y = 0 }, [])
            diffs

    List.rev lines

let parseWire (line: string) =
    line.Split ','
    |> Array.map (fun item -> parseDir item)

let parseInput fileName =
    utils.parser.readLines fileName
    |> Array.map (fun line -> parseWire line |> toLines)
