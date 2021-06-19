module utils.run

open System.Diagnostics

type problem =
    { data: (int * int * int * (string -> unit) * string) }

let run prob =
    let watch = new Stopwatch()

    watch.Start()

    let year, day, part, fn, fileName = prob.data
    let label = $"{year} Day {day}, Part {part}"

    try
        fn $"input/{year}/day{day}/{fileName}"
    with ex -> printfn $"{label} FAILED: {ex}"

    watch.Stop()

    let ms = int (watch.ElapsedMilliseconds)
    printfn $"{label} {ms} ms"

    ms

let checkResult (exp: 'a) (act: 'a) =
    if exp <> act then
        failwith $"{exp} != {act}"
