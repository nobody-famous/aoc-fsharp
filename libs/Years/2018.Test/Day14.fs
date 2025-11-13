module Aoc.Year2018.Test.Day14

open Xunit

[<Fact>]
let Part1 () =
    Assert.Equal("5158916779", Aoc.Year2018.Day14.Part1.run [ "9" ])
    Assert.Equal("0124515891", Aoc.Year2018.Day14.Part1.run [ "5" ])
    Assert.Equal("9251071085", Aoc.Year2018.Day14.Part1.run [ "18" ])
    Assert.Equal("5941429882", Aoc.Year2018.Day14.Part1.run [ "2018" ])

[<Fact>]
let Part2 () =
    Assert.Equal(9, Aoc.Year2018.Day14.Part2.run [ "51589" ])
    Assert.Equal(5, Aoc.Year2018.Day14.Part2.run [ "01245" ])
    Assert.Equal(18, Aoc.Year2018.Day14.Part2.run [ "92510" ])
    Assert.Equal(2018, Aoc.Year2018.Day14.Part2.run [ "59414" ])
