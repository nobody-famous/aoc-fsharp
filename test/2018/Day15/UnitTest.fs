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

[<Test>]
let Part1 () =
    Assert.AreEqual(10, Aoc.Year2018.Day15.Part1.run sample)

[<EntryPoint>]
let main _ = 0
