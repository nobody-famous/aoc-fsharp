module Aoc.Year2018.Test.Day13

open Xunit

let sample =
    [ """/->-\"""
      """|   |  /----\"""
      """| /-+--+-\  |"""
      """| | |  | v  |"""
      """\-+-/  \-+--/"""
      """  \------/""" ]

let sample2 =
    [ """/>-<\"""
      """|   |"""
      """| /<+-\"""
      """| | | v"""
      """\>+</ ^"""
      """  |   |"""
      """  \<->/""" ]

let sample3 =
    [ """/<--\"""
      """^   |  /----\"""
      """| /-+--+-\  |"""
      """| | |  | v  |"""
      """\-+-/  \-+--/"""
      """  \------/""" ]

[<Fact>]
let Part1 () =
    Assert.Equal("7,3", Aoc.Year2018.Day13.Part1.run sample)

[<Fact>]
let Part2 () =
    Assert.Equal("6,4", Aoc.Year2018.Day13.Part2.run sample2)
