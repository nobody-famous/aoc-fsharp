module Aoc.Year2018.Test.Day22

open Xunit

let sample = [ "depth: 510"; "target: 10,10" ]

[<Fact>]
let Part1 () =
    Assert.Equal(114, Aoc.Year2018.Day22.Part1.run sample)

[<Fact>]
let Part2 () =
    Assert.Equal(45, Aoc.Year2018.Day22.Part2.run sample)
