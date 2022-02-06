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
    Assert.AreEqual(1147, Aoc.Year2018.Day18.Part1.run sample)

[<Test>]
let Part2 () =
    Assert.AreEqual(1147, Aoc.Year2018.Day18.Part2.run sample)

[<EntryPoint>]
let main _ = 0
