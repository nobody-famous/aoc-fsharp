module Aoc.Year2018.Test.Day18

open Xunit

let sample =
    [ ".#.#...|#."
      ".....#|##|"
      ".|..|...#."
      "..|#.....#"
      "#.#|||#|#|"
      "...#.||..."
      ".|....|..."
      "||...#|.#|"
      "|.||||..|."
      "...#.|..|." ]

[<Fact>]
let Part1 () =
    Assert.Equal(1147, Aoc.Year2018.Day18.Part1.run sample)
