module utils.run

open System

type 'a ToRun =
    { label: string
      fn: unit -> 'a
      exp: 'a }

let run prob =
    let start = DateTime.Now.Millisecond
    let answer = prob.fn ()

    if prob.exp <> answer then
        printfn $"{prob.label} FAILED: {answer} != {prob.exp}"

    let diff = DateTime.Now.Millisecond - start
    printfn $"{prob.label}: {diff} ms"

    diff
