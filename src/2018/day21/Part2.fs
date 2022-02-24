module Aoc.Year2018.Day21.Part2

module U = Aoc.Year2018.Day21.Utils

let mach r0 =
    let mutable a = r0
    let mutable b = 0
    let mutable c = 0
    let mutable d = 0
    let mutable e = 0

    // #ip 5

    d <- 123

    while d <> 72 do
        d <- d &&& 456

    // 0  seti 123 0 3
// 1  bani 3 456 3
// 2  eqri 3 72 3
// 3  addr 3 5 5
// 4  seti 0 0 5

    d <- 0

    let mutable outerDone = false

    while not outerDone do
        b <- d ||| 65536
        d <- 9450265

        let mutable loop8done = false

        while not loop8done do
            e <- b &&& 255
            d <- d + e
            d <- d &&& 16777215
            d <- d * 65899
            d <- d &&& 16777215

            // 5  seti 0 9 3
// 6  bori 3 65536 1
// 7  seti 9450265 6 3
// 8  bani 1 255 4
// 9  addr 3 4 3
// 10 bani 3 16777215 3
// 11 muli 3 65899 3
// 12 bani 3 16777215 3

            if b <= 256 then

                // 13 gtir 256 1 4
// 14 addr 4 5 5
// 15 addi 5 1 5
// 16 seti 27 1 5

                e <- 0

                let mutable loop18done = false

                while not loop18done do
                    c <- e + 1
                    c <- c * 256

                    // 17 seti 0 9 4
// 18 addi 4 1 2
// 19 muli 2 256 2

                    if b <= c then

                        // 20 gtrr 2 1 2
// 21 addr 2 5 5
// 22 addi 5 1 5
// 23 seti 25 7 5

                        e <- e + 1
                    // goto 18

                    // 24 addi 4 1 4
// 25 seti 17 5 5

                    else
                        loop18done <- true

                        b <- e
            // goto 18

            // 24 addi 4 1 4
// 25 seti 17 5 5

            else
                printfn $"HERE {a} {d}"
                outerDone <- true
                loop8done <- true

                if a = d then
                    outerDone <- true
                    loop8done <- true

// 28 eqrr 3 0 4
// 29 addr 4 5 5
// 30 seti 5 8 5


let countInstrs input r0 =
    let state = U.parse input
    let mutable isDone = false

    let seen =
        System.Collections.Generic.HashSet<int>()

    while not isDone do
        if state.Mach.Registers.[state.Mach.IpReg] = 28 then
            if seen.Contains(state.Mach.Registers.[3]) then
                printfn $"{state.Mach.Registers.[3]}"
                isDone <- true

            seen.Add(state.Mach.Registers.[3]) |> ignore
        U.exec state

    state.Mach.Registers.[3]


let run (input: string) =
    let state = U.parse input

    countInstrs input 0

    // printfn $"{countInstrs input 2589}"
    // printfn $"{countInstrs input 16777107}"

    // let mutable isDone = false

    // let seen =
    //     System.Collections.Generic.HashSet<int>()

    // mach 986758

    // let mutable low = System.Int32.MaxValue
    // while not isDone do
    //     if state.Mach.Registers.[state.Mach.IpReg] = 28 then
    //         // if seen.Contains(state.Mach.Registers.[3]) then
    //         //     isDone <- true
    //         // else
    //         //     seen.Add(state.Mach.Registers.[3]) |> ignore

    //         if state.Mach.Registers.[3] < low then
    //             low <- state.Mach.Registers.[3]

    //             printfn $"{low}"

    //     U.exec state

    // state.Mach.Registers.[3]

    // 0
