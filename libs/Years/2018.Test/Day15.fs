module Aoc.Year2018.Test.Day15

open Xunit

let sample =
    [ "#######"
      "#.G...#"
      "#...EG#"
      "#.#.#G#"
      "#..G#E#"
      "#.....#"
      "#######" ]

let sample2 =
    [ "#######"
      "#G..#E#"
      "#E#E.E#"
      "#G.##.#"
      "#...#E#"
      "#...E.#"
      "#######" ]

let sample3 =
    [ "#######"
      "#E..EG#"
      "#.#G.E#"
      "#E.##E#"
      "#G..#.#"
      "#..E#.#"
      "#######" ]

let sample4 =
    [ "#######"
      "#E.G#.#"
      "#.#G..#"
      "#G.#.G#"
      "#G..#.#"
      "#...E.#"
      "#######" ]

let sample5 =
    [ "#######"
      "#.E...#"
      "#.#..G#"
      "#.###.#"
      "#E#G#G#"
      "#...#G#"
      "#######" ]

let sample6 =
    [ "#########"
      "#G......#"
      "#.E.#...#"
      "#..##..G#"
      "#...##..#"
      "#...#...#"
      "#.G...G.#"
      "#.....G.#"
      "#########" ]

[<Fact>]
let Part1 () =
    Assert.Equal(27730, Aoc.Year2018.Day15.Part1.run sample)
    Assert.Equal(36334, Aoc.Year2018.Day15.Part1.run sample2)
    Assert.Equal(39514, Aoc.Year2018.Day15.Part1.run sample3)
    Assert.Equal(27755, Aoc.Year2018.Day15.Part1.run sample4)
    Assert.Equal(28944, Aoc.Year2018.Day15.Part1.run sample5)
    Assert.Equal(18740, Aoc.Year2018.Day15.Part1.run sample6)

[<Fact>]
let Part2 () =
    Assert.Equal(4988, Aoc.Year2018.Day15.Part2.run sample)
    Assert.Equal(31284, Aoc.Year2018.Day15.Part2.run sample3)
    Assert.Equal(3478, Aoc.Year2018.Day15.Part2.run sample4)
    Assert.Equal(6474, Aoc.Year2018.Day15.Part2.run sample5)
    Assert.Equal(1140, Aoc.Year2018.Day15.Part2.run sample6)
