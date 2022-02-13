module Aoc.Year2018.Day20.Part1

type Direction =
    | N
    | S
    | E
    | W

type Node =
    | Leaf of seq<Direction>
    | Branch of seq<Node>

type Regex = list<Node>

// let parseRegex (regex: string) =
//     let re = regex.Substring(1, regex.Length - 2)

//     let updateBranch item node =
//         match node with
//         | Branch kids -> Branch(Seq.append kids (Seq.singleton item))
//         | _ -> failwith "SHOULD NOT BE HERE"

//     let rec loop (curRegex: Regex) curStack curSeg rem =
//         match rem with
//         | next :: rest ->
//             match next with
//             | 'N' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton N)) rest
//             | 'S' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton S)) rest
//             | 'E' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton E)) rest
//             | 'W' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton W)) rest
//             | '(' ->
//                 match curStack with
//                 | top :: bottom ->
//                     let newStack =
//                         (updateBranch (Leaf(curSeg)) top) :: bottom

//                     loop curRegex (Branch([]) :: newStack) Seq.empty rest
//                 | [] -> loop ((Leaf(curSeg)) :: curRegex) (Branch([]) :: curStack) Seq.empty rest
//             | '|' ->
//                 match curStack with
//                 | top :: bottom ->
//                     let newStack =
//                         (updateBranch (Leaf(curSeg)) top) :: bottom

//                     loop curRegex newStack Seq.empty rest
//                 | [] -> failwith "SHOULD NOT BE HERE"
//             | ')' ->
//                 match curStack with
//                 | top :: next :: bottom ->
//                     let newTop = updateBranch (Leaf(curSeg)) top
//                     let newNext = updateBranch newTop next
//                     let newStack = newNext :: bottom

//                     loop curRegex newStack Seq.empty rest
//                 | [ top ] ->
//                     let newTop = updateBranch (Leaf(curSeg)) top

//                     loop (newTop :: curRegex) [] Seq.empty rest
//                 | [] -> failwith "SHOULD NOT BE HERE"
//             | _ -> failwith "SHOULD NOT BE HERE"
//         | _ ->
//             let newRegex =
//                 if Seq.isEmpty curSeg then
//                     curRegex
//                 else
//                     Leaf(curSeg) :: curRegex

//             List.rev newRegex

//     let result = loop [] [] Seq.empty (Seq.toList re)

//     printfn $"RESULT"

//     for item in result do
//         printfn $"  {item}"

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
