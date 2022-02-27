module Aoc.Year2018.Day24.Part1

module S = Aoc.Utils.String

type Resistance = System.Collections.Generic.Dictionary<string, string>

type Group =
    { ArmyName: string
      Id: string
      Units: int
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

let parseGroup name id (line: string) =
    let rx =
        Regex(
            @"(\d+) units each with (\d+) hit points \((.*)\) with an attack that does (\d+) (.*?) damage at initiative (\d+)"
        )

    let matches = rx.Matches(line)

    matches
    |> Seq.fold
        (fun acc m ->
            let units = int m.Groups.[1].Value
            let attackDamage = int m.Groups.[4].Value

            { ArmyName = name
              Id = $"{name} {id}"
              Units = units
              HitPoints = int m.Groups.[2].Value
              Defense = parseDefense m.Groups.[3].Value
              AttackDamage = attackDamage
              AttackType = m.Groups.[5].Value
              Initiative = int m.Groups.[6].Value }
            :: acc)
        []
    |> List.head

let parseArmy (lines: list<string>) =
    let nameRx = Regex(@"(.*):")

    let rec loop name id groups rem =
        match rem with
        | first :: rest ->
            if first.Equals("") then
                (name, groups, rest)
            else if nameRx.IsMatch(first) then
                let newName = first.Substring(0, first.Length - 1)
                loop newName id groups rest
            else
                let group = parseGroup name id first
                loop name (id + 1) (Seq.append groups (Seq.singleton group)) rest
        | _ -> (name, groups, rem)

    loop "" 1 Seq.empty lines

let parse (input: string) =
    let lines =
        input.Split '\n' |> S.trimIndent |> Array.toList

    let (firstName, firstArmy, rem) = parseArmy lines
    let (secondName, secondArmy, _) = parseArmy rem

    let findArmy name =
        if firstName.Equals(name) then
            firstArmy
        else if secondName.Equals(name) then
            secondArmy
        else
            failwith $"{name} not found"

    { ImmuneSystem = findArmy "Immune System"
      Infection = findArmy "Infection" }

type GroupSet = System.Collections.Generic.HashSet<Group>

let dealDamage (src: Group) (dst: Group) =
    let attack = src.AttackType
    let damage = src.Units * src.AttackDamage

    let defense =
        if dst.Defense.ContainsKey(attack) then
            match dst.Defense.[attack] with
            | "immune" -> 0
            | "weak" -> 2
            | _ -> 1
        else
            1

    damage * defense

let selectTargets (us: Army) (them: Army) =
    let groups =
        us
        |> Seq.sortByDescending (fun group ->
            (group.Units * group.AttackDamage) * 100
            + group.Initiative)

    let targeted = GroupSet()

    let rec loop paired remGroups =
        match remGroups with
        | first :: rest ->
            let ops =
                them
                |> Seq.filter (fun g -> not (targeted.Contains(g)))
                |> Seq.sortByDescending (fun g ->
                    (dealDamage first g) * 10000
                    + (g.Units * g.AttackDamage) * 100
                    + g.Initiative)

            let target =
                if not (Seq.isEmpty ops) then
                    let op = Seq.head ops
                    targeted.Add(op) |> ignore
                    Some(op)
                else
                    None

            let newPairs =
                Seq.append paired (Seq.singleton (first, target))

            loop newPairs rest
        | [] -> paired

    loop Seq.empty (groups |> Seq.toList)

let doDamage src dst =
    let damage = dealDamage src dst
    let killed = min dst.Units (damage / dst.HitPoints)

    printfn $"doDamage {src.Id} -> {dst.Id} {damage} {dst.HitPoints} {damage / dst.HitPoints} {killed}"

    dst

let doFight (fight: Fight) =
    let battles =
        Seq.append (selectTargets fight.Infection fight.ImmuneSystem) (selectTargets fight.ImmuneSystem fight.Infection)
        |> Seq.sortByDescending (fun (src, _) -> src.Initiative)
        |> Seq.map (fun (src, dst) ->
            match dst with
            | Some t -> Some(doDamage src t)
            | None -> None)

    for target in battles do
        match target with
        | Some t -> printfn $"t {t}"
        | None -> ()

    0

let run (input: string) =
    let armies = parse input

    doFight armies |> ignore

    // let toFight =
    //     Seq.append
    //         (selectTargets armies.Infection armies.ImmuneSystem)
    //         (selectTargets armies.ImmuneSystem armies.Infection)
    //     |> Seq.sortByDescending (fun (src, _) -> src.Initiative)

    // for (src, dst) in toFight do
    //     printfn $"{src} -> {dst}"

    0
