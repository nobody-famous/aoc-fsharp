module Aoc.Year2018.Test.Day9

open Xunit

[<Fact>]
let Part1 () =
    Assert.Equal(32L, Aoc.Year2018.Day9.Part1.run [ "9 players; last marble is worth 25 points" ])
    Assert.Equal(8317L, Aoc.Year2018.Day9.Part1.run [ "10 players; last marble is worth 1618 points" ])
    Assert.Equal(146373L, Aoc.Year2018.Day9.Part1.run [ "13 players; last marble is worth 7999 points" ])
    Assert.Equal(2764L, Aoc.Year2018.Day9.Part1.run [ "17 players; last marble is worth 1104 points" ])
    Assert.Equal(54718L, Aoc.Year2018.Day9.Part1.run [ "21 players; last marble is worth 6111 points" ])
    Assert.Equal(37305L, Aoc.Year2018.Day9.Part1.run [ "30 players; last marble is worth 5807 points" ])
