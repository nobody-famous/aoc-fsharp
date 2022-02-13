module Aoc.Year2018.Day20.Part1

type Direction =
    | N
    | S
    | E
    | W

let charToDir ch =
    match ch with
    | 'N' -> N
    | 'S' -> S
    | 'E' -> E
    | 'W' -> W
    | _ -> failwith $"charToDir: {ch}"

let addToSegment dir seg = Seq.append seg (Seq.singleton dir)

let segToString seg =
    seg |> Seq.map string |> String.concat ""

let parseSegment input =
    let rec loop curSeg rem =
        match rem with
        | first :: rest ->
            match first with
            | 'N'
            | 'S'
            | 'E'
            | 'W' -> loop (addToSegment (charToDir first) curSeg) rest
            | _ -> (curSeg, rem)
        | [] -> (curSeg, [])

    loop Seq.empty input

let rec parseBranch paths input =
    let rec loop isOpen curPaths outPaths rem =
        match rem with
        | first :: rest ->
            match first with
            | '(' ->
                if not isOpen then
                    loop true curPaths outPaths rest
                else
                    let (paths, newRem) = parseBranch curPaths rest

                    loop isOpen paths outPaths newRem
            | ')' ->
                let newOutPaths =
                    if Seq.isEmpty curPaths then
                        Seq.append outPaths paths
                    else
                        Seq.append outPaths curPaths

                (newOutPaths, rest)
            | '|' ->
                let newOutPaths = Seq.append outPaths curPaths
                loop isOpen Seq.empty newOutPaths rest
            | _ ->
                let (seg, newRem) = parseSegment rem

                let newCurPaths =
                    Seq.append curPaths (Seq.map (fun s -> Seq.append s seg) paths)

                loop isOpen newCurPaths outPaths newRem
        | _ -> (outPaths, rem)

    loop false Seq.empty Seq.empty input

let parseRegex (input: string) =
    let rec loop paths rem =
        match rem with
        | first :: rest ->
            match first with
            | '^' -> loop Seq.empty rest
            | '$' -> paths
            | 'N'
            | 'S'
            | 'E'
            | 'W' ->
                let (seg, newRem) = parseSegment rem

                let newPaths =
                    if Seq.isEmpty paths then
                        Seq.append paths (Seq.singleton seg)
                    else
                        Seq.map (fun s -> Seq.append s seg) paths

                loop newPaths newRem
            | '(' ->
                let (newPaths, newRem) = parseBranch paths rem

                loop newPaths newRem
            | _ -> failwith $"Invalid input {input}"
        | [] -> paths

    loop Seq.empty (input |> Seq.toList)

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> parseRegex

let run (input: string) =
    let paths = parse input

    printfn "PATHS"
    for path in paths do
        printfn $"  {segToString path}"
