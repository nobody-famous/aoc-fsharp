module Aoc.Year2018.Day2.Part2

let countDiffs (str1: string) (str2: string) =
    let rec loop count s1 s2 =
        match s1, s2 with
        | f1 :: r1, f2 :: r2 ->
            if f1 = f2 then
                loop count r1 r2
            else
                loop (count + 1) r1 r2
        | _, _ -> count

    loop 0 (Seq.toList str1) (Seq.toList str2)

let findIds diffCount (ids: string array) =
    let rec loop strs =
        match strs with
        | first :: second :: _ ->
            if countDiffs first second = diffCount then
                (first, second)
            else
                loop (List.tail strs)
        | _ -> failwith "Did not find it"

    loop (Seq.toList ids)

let removeDiff (str1, str2) =
    let rec loop out s1 s2 =
        match s1, s2 with
        | f1 :: r1, f2 :: r2 ->
            if f1 = f2 then
                loop $"{out}{f1}" r1 r2
            else
                loop out r1 r2
        | _, _ -> out

    loop "" (Seq.toList str1) (Seq.toList str2)

let run exp fileName =
    Parser.parseInput fileName
    |> Array.sort
    |> findIds 1
    |> removeDiff
    |> Utils.Run.checkResult exp
