module Day9

open NUnit.Framework

[<Test>]
let Part1 () =
    Assert.AreEqual(32, y2018.day9.part1.run "9 players; last marble is worth 25 points")
    Assert.AreEqual(8317, y2018.day9.part1.run "10 players; last marble is worth 1618 points")
    Assert.AreEqual(146373, y2018.day9.part1.run "13 players; last marble is worth 7999 points")
    Assert.AreEqual(2764, y2018.day9.part1.run "17 players; last marble is worth 1104 points")
    Assert.AreEqual(54718, y2018.day9.part1.run "21 players; last marble is worth 6111 points")
    Assert.AreEqual(37305, y2018.day9.part1.run "30 players; last marble is worth 5807 points")

[<EntryPoint>]
let main _ = 0
