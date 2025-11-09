open Aoc.Utils.Runner

module S = Aoc.Utils.String

let problems: Problem list =
    [ IntProblem("2018/day1/part1", Aoc.Year2018.Day1.Part1.run, 437)
      IntProblem("2018/day1/part2", Aoc.Year2018.Day1.Part2.run, 655)
      LongProblem("2018/day9/part1", Aoc.Year2018.Day9.Part1.run, 382055)
      LongProblem("2018/day9/part2", Aoc.Year2018.Day9.Part2.run, 3133277384L)
      StringProblem("2018/day13/part1", Aoc.Year2018.Day13.Part1.run, "80,100")
      StringProblem("2018/day13/part2", Aoc.Year2018.Day13.Part2.run, "16,99")
      StringProblem("2018/day14/part1", Aoc.Year2018.Day14.Part1.run, "9276422810")
      IntProblem("2018/day14/part2", Aoc.Year2018.Day14.Part2.run, 20319117)
      IntProblem("2018/day15/part1", Aoc.Year2018.Day15.Part1.run, 269430)
      IntProblem("2018/day15/part2", Aoc.Year2018.Day15.Part2.run, 55160)
      IntProblem("2018/day16/part1", Aoc.Year2018.Day16.Part1.run, 605)
      IntProblem("2018/day16/part2", Aoc.Year2018.Day16.Part2.run, 653)
      IntProblem("2018/day17/part1", Aoc.Year2018.Day17.Part1.run, 37649)
      IntProblem("2018/day17/part2", Aoc.Year2018.Day17.Part2.run, 30112)
      IntProblem("2018/day18/part1", Aoc.Year2018.Day18.Part1.run, 638400)
      IntProblem("2018/day18/part2", Aoc.Year2018.Day18.Part2.run, 195952)
      IntProblem("2018/day19/part1", Aoc.Year2018.Day19.Part1.run, 948)
      IntProblem("2018/day19/part2", Aoc.Year2018.Day19.Part2.run, 10695960)
      IntProblem("2018/day20/part1", Aoc.Year2018.Day20.Part1.run, 4432)
      IntProblem("2018/day20/part2", Aoc.Year2018.Day20.Part2.run, 8681)
      IntProblem("2018/day21/part1", Aoc.Year2018.Day21.Part1.run, 986758)
      IntProblem("2018/day21/part2", Aoc.Year2018.Day21.Part2.run, 16016565) ]

let args = System.Environment.GetCommandLineArgs()

let path =
    if Array.length args > 1 then
        args.[1]
    else
        "UNKNOWN_PATH"

let printString s = printfn $"{s}"

let total =
    problems
    |> List.map (fun p ->
        let msg, ms = runProblem path p
        printString msg
        ms)
    |> List.sum

printfn $"Total {total} ms"
