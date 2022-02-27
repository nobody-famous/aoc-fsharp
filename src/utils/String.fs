module Aoc.Utils.String

let trimIndent (input: string array) : string array =
    let strIndent (str: string) =
        str.ToCharArray()
        |> Array.takeWhile (fun ch -> System.Char.IsWhiteSpace ch)

    let stripIndent (input: string array) : string array =
        let indent =
            Array.fold
                (fun acc (str: string) ->
                    if str.Length = 0 then
                        acc
                    else
                        min acc (strIndent str).Length)
                System.Int32.MaxValue
                input

        Array.map
            (fun (str: string) ->
                if str.Length > indent then
                    str.Substring indent
                else
                    str)
            input

    let findEnds (input: string array) : int * int =
        let mutable startNdx = 0
        let mutable endNdx = input.Length - 1

        while input.[startNdx].Trim().Length = 0 do
            startNdx <- startNdx + 1

        while input.[endNdx].Trim().Length = 0 do
            endNdx <- endNdx - 1

        (startNdx, endNdx)

    let cutEnds (input: string array) startNdx endNdx : string array =
        Array.sub input startNdx (endNdx - startNdx + 1)
        |> Array.map (fun str -> str.TrimEnd())

    input |> findEnds ||> cutEnds input |> stripIndent
