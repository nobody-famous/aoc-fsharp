module Day16

open NUnit.Framework

let sample =
    """
    Before: [3, 2, 1, 1]
    9 2 1 2
    After:  [3, 2, 2, 1]
    """

[<Test>]
let Part1 () =
    Assert.AreEqual(1, Aoc.Year2018.Day16.Part1.run sample)

[<EntryPoint>]
let main _ = 0
