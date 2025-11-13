module Aoc.Year2018.Test.Day19

open Xunit

let sample =
    [ "#ip 0"
      "seti 5 0 1"
      "seti 6 0 2"
      "addi 0 1 0"
      "addr 1 2 3"
      "setr 1 0 0"
      "seti 8 0 4"
      "seti 9 0 5" ]

[<Fact>]
let Part1 () =
    // Assert.Equal(7, Aoc.Year2018.Day19.Part1.run sample)
    Assert.Equal(948, Aoc.Year2018.Day19.Part1.run sample)
