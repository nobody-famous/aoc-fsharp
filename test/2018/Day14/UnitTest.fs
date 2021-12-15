module Day14

open NUnit.Framework

[<Test>]
let Part1 () =
    Assert.AreEqual("5158916779", Aoc.Year2018.Day14.Part1.run "9")
    Assert.AreEqual("0124515891", Aoc.Year2018.Day14.Part1.run "5")
    Assert.AreEqual("9251071085", Aoc.Year2018.Day14.Part1.run "18")
    Assert.AreEqual("5941429882", Aoc.Year2018.Day14.Part1.run "2018")

[<EntryPoint>]
let main _ = 0
