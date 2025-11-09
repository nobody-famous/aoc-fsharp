module Aoc.Year2018.Day23.Part2

module U = Aoc.Year2018.Day23.Utils
module G = Aoc.Utils.Geometry

type Queue = System.Collections.Generic.PriorityQueue<int, int>

let dequeue (q: Queue) =
    let mutable elem = 0
    let mutable weight = 0

    q.TryDequeue(&elem, &weight) |> ignore
    elem, weight

let run (input: string list) =
    let bots = U.parse input

    let pq = Queue()

    bots
    |> List.iter (fun bot ->
        let dist = G.manDist3d bot.Pos G.origin3d

        pq.Enqueue(1, max 0 (dist - bot.Radius))
        pq.Enqueue(-1, dist + bot.Radius + 1))

    let mutable count = 0
    let mutable maxCount = 0
    let mutable result = 0

    while pq.Count > 0 do
        let value, dist = dequeue pq

        count <- count + value

        if count > maxCount then
            result <- dist
            maxCount <- count

    result
