module Aoc.Year2018.Test.Day1

open Xunit

let sample = [ "+1"; "-2"; "+3"; "+1" ]

[<Fact>]
let Part1 () =
    let answer = Aoc.Year2018.Day1.Part1.run sample

    Assert.Equal(3, answer)

[<Fact>]
let Part2 () =
    let answer = Aoc.Year2018.Day1.Part2.run sample

    Assert.Equal(2, answer)
