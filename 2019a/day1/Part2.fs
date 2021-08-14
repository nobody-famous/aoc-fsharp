module Aoc.Year2019.Day1.Part2

let totalFuel mass =
    let rec loop total cur =
        let next = Utils.calcFuel cur

        if next <= 0 then
            total
        else
            loop (total + next) next

    loop 0 mass

let run exp fileName =
    Parser.parseInput fileName
    |> Array.map totalFuel
    |> Array.sum
    |> Aoc.Utils.Run.checkResult exp
