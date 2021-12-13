module Day13

open NUnit.Framework

let sample =
    """
    /->-\
    |   |  /----\
    | /-+--+-\  |
    | | |  | v  |
    \-+-/  \-+--/
      \------/
    """

[<Test>]
let Part1 () =
    Assert.AreEqual("7,3",Aoc.Year2018.Day13.Part1.run sample)
    Assert.Pass()

[<EntryPoint>]
let main _ = 0
