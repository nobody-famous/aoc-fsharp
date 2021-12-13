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

let sample2 =
    """
    />-<\  
    |   |  
    | /<+-\
    | | | v
    \>+</ |
      |   ^
      \<->/
      """

[<Test>]
let Part1 () =
    Assert.AreEqual("7,3",Aoc.Year2018.Day13.Part1.run sample)
    Assert.Pass()

[<Test>]
let Part2 () =
    Assert.AreEqual("6,4",Aoc.Year2018.Day13.Part2.run sample2)
    Assert.Pass()

[<EntryPoint>]
let main _ = 0
