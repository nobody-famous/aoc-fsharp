module Day1

open NUnit.Framework

[<SetUp>]
let Setup () = ()

let sample =
    "
    +1
    -2
    +3
    +1
    "

[<Test>]
let Part1 () =
    let answer = Aoc.Year2018.Day1.Part1.run sample

    Assert.AreEqual(3, answer)

[<Test>]
let Part2 () =
    let answer = Aoc.Year2018.Day1.Part2.run sample

    Assert.AreEqual(2, answer)

[<EntryPoint>]
let main _ = 0
