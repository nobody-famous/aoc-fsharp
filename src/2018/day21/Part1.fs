module Aoc.Year2018.Day21.Part1

module U = Aoc.Year2018.Day21.Utils

let run (input: string) =
    let state = U.parse input

    U.mach 986758
    // while state.Mach.Registers.[state.Mach.IpReg] <> 28 do
    //     state.Mach.Registers.[0] <- state.Mach.Registers.[3]
    //     U.exec state

    // state.Mach.Registers.[3]

// 0  seti 123 0 3      r3 = 123
// 1  bani 3 456 3      r3 = r3 & 456
// 2  eqri 3 72 3       r3 = (r3 == 72)
// 3  addr 3 5 5        ip = ip + r3
// 4  seti 0 0 5        ip = 0
// 5  seti 0 9 3        r3 = 0
// 6  bori 3 65536 1    r1 = r3 | 65536
// 7  seti 9450265 6 3  r3 = 9450265
// 8  bani 1 255 4      r4 = r1 & 255
// 9  addr 3 4 3        r3 = r3 + r4
// 10 bani 3 16777215 3 r3 = r3 & 16777215
// 11 muli 3 65899 3    r3 = r3 * 65899
// 12 bani 3 16777215 3 r3 = r3 & 16777215
// 13 gtir 256 1 4      r4 = (256 > r1)
// 14 addr 4 5 5        ip = ip + r4
// 15 addi 5 1 5        ip = ip + 2
// 16 seti 27 1 5       ip = 28
// 17 seti 0 9 4        r4 = 0
// 18 addi 4 1 2        r2 = r4 + 1
// 19 muli 2 256 2      r2 = r2 * 256
// 20 gtrr 2 1 2        r2 = (r2 > r1)
// 21 addr 2 5 5        ip = ip + r2
// 22 addi 5 1 5        ip = ip + 2
// 23 seti 25 7 5       ip = 26
// 24 addi 4 1 4        r4 = r4 + 1
// 25 seti 17 5 5       ip = 18
// 26 setr 4 6 1        r1 = r4
// 27 seti 7 8 5        ip = 8
// 28 eqrr 3 0 4        r4 = (r3 == r0)
// 29 addr 4 5 5        ip = ip + r4
// 30 seti 5 8 5        ip = 6



// 0  r3 = 123
// 1  r3 = r3 & 456
// 2  r3 = (r3 == 72)
// 3  ip = ip + r3
// 4  ip = 0
// 5  r3 = 0
// 6  r1 = r3 | 65536
// 7  r3 = 9450265
// 8  r4 = r1 & 255
// 9  r3 = r3 + r4
// 10 r3 = r3 & 16777215
// 11 r3 = r3 * 65899
// 12 r3 = r3 & 16777215
// 13 r4 = (256 > r1)
// 14 ip = ip + r4
// 15 ip = ip + 2
// 16 ip = 28
// 17 r4 = 0
// 18 r2 = r4 + 1
// 19 r2 = r2 * 256
// 20 r2 = (r2 > r1)
// 21 ip = ip + r2
// 22 ip = ip + 2
// 23 ip = 26
// 24 r4 = r4 + 1
// 25 ip = 18
// 26 r1 = r4
// 27 ip = 8
// 28 r4 = (r3 == r0)
// 29 ip = ip + r4
// 30 ip = 6



