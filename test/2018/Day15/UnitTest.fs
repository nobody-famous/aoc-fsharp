module Day15

open NUnit.Framework

let sample =
    """
    #######
    #.G...#
    #...EG#
    #.#.#G#
    #..G#E#
    #.....#
    #######
    """

let sample2 =
    """
    #######
    #G..#E#
    #E#E.E#
    #G.##.#
    #...#E#
    #...E.#
    #######
    """

let sample3 =
    """
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######
    """

let sample4 =
    """
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######
    """

let sample5 =
    """
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######
    """

let sample6 =
    """
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########
    """

[<Test>]
let Part1 () =
    // Assert.AreEqual(27730, Aoc.Year2018.Day15.Part1.run sample)
    // Assert.AreEqual(36334, Aoc.Year2018.Day15.Part1.run sample2)
    // Assert.AreEqual(39514, Aoc.Year2018.Day15.Part1.run sample3)
    // Assert.AreEqual(27755, Aoc.Year2018.Day15.Part1.run sample4)
    // Assert.AreEqual(28944, Aoc.Year2018.Day15.Part1.run sample5)
    Assert.AreEqual(18740, Aoc.Year2018.Day15.Part1.run sample6)

[<EntryPoint>]
let main _ = 0
