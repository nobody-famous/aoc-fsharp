module Aoc.Year2018.Day24.Part1

type Resistance = System.Collections.Generic.Dictionary<string, string>

type Group =
    { Units: int
      HitPoints: int
      Defense: Resistance
      AttackType: string
      AttackDamage: int
      Initiative: int }

type Army = seq<Group>
type Regex = System.Text.RegularExpressions.Regex

type Fight = { ImmuneSystem: Army; Infection: Army }

let parseDefense (str: string) =
    let defense = Resistance()

    str.Split ';'
    |> Array.map (fun s -> s.Trim())
    |> Array.iter (fun s ->
        let rx = Regex(@"(.*?) to (.*)")
        let matches = rx.Matches(s)

        matches
        |> Seq.iter (fun m ->
            m.Groups.[2].Value.Split ','
            |> Seq.map (fun s -> s.Trim())
            |> Seq.iter (fun s -> defense.[s] <- m.Groups.[1].Value)))

    defense

let parseGroup (line: string) =
    let rx =
        Regex(
            @"(\d+) units each with (\d+) hit points \((.*)\) with an attack that does (\d+) (.*?) damage at initiative (\d+)"
        )

    let matches = rx.Matches(line)

    matches
    |> Seq.fold
        (fun acc m ->
            { Units = int m.Groups.[1].Value
              HitPoints = int m.Groups.[2].Value
              Defense = parseDefense m.Groups.[3].Value
              AttackDamage = int m.Groups.[4].Value
              AttackType = m.Groups.[5].Value
              Initiative = int m.Groups.[6].Value }
            :: acc)
        []
    |> List.head

let parseArmy (lines: string []) = 0

let run (input: string) =
    let group =
        parseGroup
            "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"

    printfn $"{group}"
    0
