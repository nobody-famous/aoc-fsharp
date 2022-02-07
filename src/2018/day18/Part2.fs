module Aoc.Year2018.Day18.Part2

module U = Aoc.Year2018.Day18.Utils

type BoardMap = System.Collections.Generic.Dictionary<int, int>

let hashPoint x y = x + y

let hashBoard (board: U.Board) =
    let mutable hash = 0

    for x in 0 .. board.GetLength(0) - 1 do
        for y in 0 .. board.GetLength(1) - 1 do
            hash <-
                hash
                + match board.[x, y] with
                  | U.Empty -> hashPoint x y
                  | U.Tree -> hashPoint (x * 100) (y * 100)
                  | U.Yard -> hashPoint (x * 1000) (y * 1000)

    hash

let run (input: string) =
    let board = U.parse input

    let target = 1000000000

    let rec loop (seen: BoardMap) count endCount curBoard =
        let hash = hashBoard curBoard

        if count = endCount then
            (0, curBoard)
        else if seen.ContainsKey hash then
            let start = seen.[hash]
            let newTarget = target - start
            let cycle = count - start
            let cycleCount = newTarget / cycle
            let remSteps = newTarget - (cycle * cycleCount)

            (remSteps, curBoard)
        else
            seen.[hash] <- count

            loop seen (count + 1) endCount (U.runMinute curBoard)

    board
    |> loop (BoardMap()) 0 target
    ||> loop (BoardMap()) 0
    |> snd
    |> U.getResourceValue
