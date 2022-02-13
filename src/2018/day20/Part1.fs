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

let rec parseBranch input =
    let rec loop isOpen rem =
        match rem with
        | first :: rest ->
            match first with
            | '(' ->
                if not isOpen then
                    loop true rest
                else
                    let (_, newRem) = parseBranch rest
                    loop isOpen newRem
            | ')' -> (Seq.empty, rest)
            | '|' -> loop isOpen rest
            | _ ->
                let (seg, newRem) = parseSegment rem

                printfn $"SEG {segToString seg} {newRem}"
                loop isOpen newRem
        | _ -> (Seq.empty, rem)

    loop false input

let parseRegex (input: string) =
    let rec loop depth curSeg rem =
        printfn $"LOOP {depth} {rem}"

        match rem with
        | first :: rest ->
            match first with
            | '^' -> loop 0 Seq.empty rest
            | '$' -> ()
            | 'N'
            | 'S'
            | 'E'
            | 'W' -> loop depth (addToSegment (charToDir first) curSeg) rest
            | '(' -> loop (depth + 1) Seq.empty rest
            | '|' -> loop depth Seq.empty rest
            | ')' -> loop (depth - 1) Seq.empty rest
            | _ -> failwith $"NOT DONE {segToString curSeg} {depth} {first} {rest}"
        | [] -> ()

    loop 0 Seq.empty (input |> Seq.toList)

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> parseRegex

let run (input: string) =
    // let regex = parse input

    // printfn $"REGEX {regex}"

    let (seg, rem) = parseSegment (Seq.toList "NSEW(WESN)")

    let (branch, rem) =
        parseBranch (Seq.toList "(NSEW|NNN(EEE))")

    printfn $"{segToString seg} {rem}"
