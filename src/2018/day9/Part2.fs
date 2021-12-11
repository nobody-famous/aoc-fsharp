module y2018.day9.part2

let updateLastMarble config =
    { config with utils.LastMarble = config.LastMarble * 100 }

let run (input: string) =
    utils.parse input
    |> updateLastMarble
    |> utils.playGame
