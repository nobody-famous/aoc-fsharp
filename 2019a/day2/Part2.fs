module Aoc.Year2019.Day2.Part2

open Aoc.Year2019.Utils.Intcode

let runMachine prog noun verb =
    Machine prog
    |> setPosition 1 noun
    |> setPosition 2 verb
    |> execAll
    |> getPosition 0

let findInputs target prog =
    let rec loop noun verb =
        let value = runMachine prog noun verb

        if value = target then
            (noun, verb)
        else
            let verb' = (verb + 1) % 100
            let noun' = if verb' = 0 then noun + 1 else noun
            loop noun' verb'

    loop 0 0

let toAnswer (noun, verb) = (noun * 100) + verb

let run exp fileName =
    parseInput fileName
    |> findInputs 19690720
    |> toAnswer
    |> Aoc.Utils.Run.checkResult exp
