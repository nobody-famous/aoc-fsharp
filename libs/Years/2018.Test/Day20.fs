module Aoc.Year2018.Test.Day20

open Xunit

let sample = [ "^WNE$" ]
let sample2 = [ "^ENWWW(NEEE|SSE(EE|N))$" ]

let sample3 =
    [ "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" ]

let sample4 =
    [ "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" ]

let sample5 =
    [ "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" ]

let sample6 = [ "^(SSS|EEESSSWWW)ENNES$" ]
let sample7 = [ "^(E|SSEENNW)S$" ]
let sample8 = [ "^(E|SEN)$" ]

[<Fact>]
let Part1 () =
    Assert.Equal(3, Aoc.Year2018.Day20.Part1.run sample)
    Assert.Equal(10, Aoc.Year2018.Day20.Part1.run sample2)
    Assert.Equal(18, Aoc.Year2018.Day20.Part1.run sample3)
    Assert.Equal(23, Aoc.Year2018.Day20.Part1.run sample4)
    Assert.Equal(31, Aoc.Year2018.Day20.Part1.run sample5)
    Assert.Equal(8, Aoc.Year2018.Day20.Part1.run sample6)
    Assert.Equal(4, Aoc.Year2018.Day20.Part1.run sample7)
    Assert.Equal(2, Aoc.Year2018.Day20.Part1.run sample8)
