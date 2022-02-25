module Day23

open NUnit.Framework

let sample =
    """
    pos=<0,0,0>, r=4
    pos=<1,0,0>, r=1
    pos=<4,0,0>, r=3
    pos=<0,2,0>, r=1
    pos=<0,5,0>, r=3
    pos=<0,0,3>, r=1
    pos=<1,1,1>, r=1
    pos=<1,1,2>, r=1
    pos=<1,3,1>, r=1
    """

[<Test>]
let Part1 () =
    Assert.AreEqual(7, Aoc.Year2018.Day23.Part1.run sample)

[<EntryPoint>]
let main _ = 0
