module Day22

open NUnit.Framework

let sample =
    """
    depth: 510
    target: 10,10
    """

[<Test>]
let Part1 () =
    Assert.AreEqual(114, Aoc.Year2018.Day22.Part1.run sample)

[<Test>]
let Part2 () =
    Assert.AreEqual(45, Aoc.Year2018.Day22.Part2.run sample)

[<EntryPoint>]
let main _ = 0
