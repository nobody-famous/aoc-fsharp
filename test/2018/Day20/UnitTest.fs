module Day20

open NUnit.Framework

let sample = "^WNE$"
let sample2 = "^ENWWW(NEEE|SSE(EE|N))$"
let sample3 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
let sample4 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
let sample5 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

[<Test>]
let Part1 () =
    // Assert.AreEqual(3, Aoc.Year2018.Day20.Part1.run sample)
    // Assert.AreEqual(10, Aoc.Year2018.Day20.Part1.run sample2)
    // Assert.AreEqual(18, Aoc.Year2018.Day20.Part1.run sample3)
    // Assert.AreEqual(23, Aoc.Year2018.Day20.Part1.run sample4)
    Assert.AreEqual(31, Aoc.Year2018.Day20.Part1.run sample5)

[<EntryPoint>]
let main _ = 0
