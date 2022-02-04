module Day17

open NUnit.Framework

let sample =
    """
    x=495, y=2..7
    y=7, x=495..501
    x=501, y=3..7
    x=498, y=2..4
    x=506, y=1..2
    x=498, y=10..13
    x=504, y=10..13
    y=13, x=498..504
    """

let sample2 =
    """
    y=5, x=499..509
    x=499, y=3..5
    x=509, y=2..5
    y=15, x=496..508
    x=496, y=10..15
    x=508, y=10..15
    y=13, x=499..501
    x=499, y=11..13
    x=501, y=11..13
    y=22, x=490..510
    x=490, y=18..22
    x=510, y=18..22
    x=495, y=18..19
    x=509, y=18..19
    """

[<Test>]
let Part1 () =
    // Assert.AreEqual(57, Aoc.Year2018.Day17.Part1.run sample)
    Assert.AreEqual(57, Aoc.Year2018.Day17.Part1.run sample2)

[<EntryPoint>]
let main _ = 0
