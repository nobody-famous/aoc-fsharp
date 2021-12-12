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
    Assert.AreEqual("7,3", y2018.day13.part1.run sample)
    Assert.Pass()

[<EntryPoint>]
let main _ = 0
