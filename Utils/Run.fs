module utils.run

open System
open System.Diagnostics

type 'a ToRun =
    { label: string
      fn: unit -> 'a
      exp: 'a }

let run prob =
    let watch = new Stopwatch()

    watch.Start()

    let answer = prob.fn ()

    watch.Stop()

    if prob.exp <> answer then
        printfn $"{prob.label} FAILED: {answer} != {prob.exp}"

    let ms = int (watch.ElapsedMilliseconds)
    printfn $"{prob.label}: {ms} ms"

    ms
