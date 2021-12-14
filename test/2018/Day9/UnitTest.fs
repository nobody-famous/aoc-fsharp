module Day9

open NUnit.Framework

[<Test>]
let Part1 () =
    Assert.AreEqual(32, Aoc.Year2018.Day9.Part1.run "9 players; last marble is worth 25 points")
    Assert.AreEqual(8317, Aoc.Year2018.Day9.Part1.run "10 players; last marble is worth 1618 points")
    Assert.AreEqual(146373, Aoc.Year2018.Day9.Part1.run "13 players; last marble is worth 7999 points")
    Assert.AreEqual(2764, Aoc.Year2018.Day9.Part1.run "17 players; last marble is worth 1104 points")
    Assert.AreEqual(54718, Aoc.Year2018.Day9.Part1.run "21 players; last marble is worth 6111 points")
    Assert.AreEqual(37305, Aoc.Year2018.Day9.Part1.run "30 players; last marble is worth 5807 points")

[<EntryPoint>]
let main _ = 0
