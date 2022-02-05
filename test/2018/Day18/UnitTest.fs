module Day18

open NUnit.Framework

let sample =
    """
    .#.#...|#.
    .....#|##|
    .|..|...#.
    ..|#.....#
    #.#|||#|#|
    ...#.||...
    .|....|...
    ||...#|.#|
    |.||||..|.
    ...#.|..|.
    """

[<Test>]
let Part1 () =
    Assert.AreEqual(1, Aoc.Year2018.Day18.Part1.run sample)

[<EntryPoint>]
let main _ = 0
