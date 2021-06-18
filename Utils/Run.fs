module utils.run

open System.Diagnostics

type 'a problem =
    { data: (int * int * int * 'a * (string -> 'a) * string) }

let run prob =
    let watch = new Stopwatch()

    watch.Start()

    let year, day, part, exp, fn, fileName = prob.data
    let label = $"{year} Day {day}, Part {part}"
    let answer = fn $"input/{year}/day{day}/{fileName}"

    watch.Stop()

    if exp <> answer then
        printfn $"{label} FAILED: {answer} != {exp}"

    let ms = int (watch.ElapsedMilliseconds)
    printfn $"{label} {ms} ms"

    ms
