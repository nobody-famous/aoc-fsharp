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

let parseSegment (regex: string) (startNdx: int) = ""

let parseRegex (regex: string) =
    let re = regex.Substring(1, regex.Length - 2)

    let updateBranch item node =
        match node with
        | Branch kids -> Branch(Seq.append kids (Seq.singleton item))
        | _ -> failwith "SHOULD NOT BE HERE"

    let rec loop (curRegex: Regex) curStack curSeg rem =
        match rem with
        | next :: rest ->
            match next with
            | 'N' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton N)) rest
            | 'S' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton S)) rest
            | 'E' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton E)) rest
            | 'W' -> loop curRegex curStack (Seq.append curSeg (Seq.singleton W)) rest
            | '(' ->
                match curStack with
                | top :: bottom ->
                    let newStack =
                        (updateBranch (Leaf(curSeg)) top) :: bottom

                    loop curRegex (Branch([]) :: newStack) Seq.empty rest
                | [] -> loop ((Leaf(curSeg)) :: curRegex) (Branch([]) :: curStack) Seq.empty rest
            | '|' ->
                match curStack with
                | top :: bottom ->
                    let newStack =
                        (updateBranch (Leaf(curSeg)) top) :: bottom

                    loop curRegex newStack Seq.empty rest
                | [] -> failwith "SHOULD NOT BE HERE"
            | ')' ->
                match curStack with
                | top :: next :: bottom ->
                    let newTop = updateBranch (Leaf(curSeg)) top
                    let newNext = updateBranch newTop next
                    let newStack = newNext :: bottom

                    loop curRegex newStack Seq.empty rest
                | [ top ] ->
                    let newTop = updateBranch (Leaf(curSeg)) top

                    loop (newTop :: curRegex) [] Seq.empty rest
                | [] -> failwith "SHOULD NOT BE HERE"
            | _ -> failwith "SHOULD NOT BE HERE"
        | _ ->
            let newRegex =
                if Seq.isEmpty curSeg then
                    curRegex
                else
                    Leaf(curSeg) :: curRegex

            List.rev newRegex

    let result = loop [] [] Seq.empty (Seq.toList re)

    printfn $"RESULT"

    for item in result do
        printfn $"  {item}"

let parse (input: string) =
    input.Split '\n'
    |> Array.toList
    |> List.head
    |> parseRegex

let run (input: string) =
    let regex = parse input

    printfn $"REGEX {regex}"
