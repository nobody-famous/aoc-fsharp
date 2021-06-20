module Aoc.Utils.Run

open System.Diagnostics

type Problem =
    { Data: (int * int * int * (string -> unit) * string) }

let run prob =
    let watch = Stopwatch()

    watch.Start()

    let year, day, part, fn, fileName = prob.Data
    let label = $"{year} Day {day}, Part {part}"

    try
        fn $"input/{year}/day{day}/{fileName}"
    with
    | ex -> printfn $"{label} FAILED: {ex}"

    watch.Stop()

    let ms = int (watch.ElapsedMilliseconds)
    printfn $"{label} {ms} ms"

    ms

let checkResult (exp: 'a) (act: 'a) =
    if exp <> act then
        failwith $"{exp} != {act}"
