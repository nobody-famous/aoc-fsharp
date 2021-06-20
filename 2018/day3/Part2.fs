module Aoc.Year2018.Day3.Part2

let overlapped (rect: Utils.Rectangle) (grid: System.Int32 [,]) =
    let overlapped = ref false

    for row in rect.Loc.Y .. rect.Loc.Y + rect.Dims.Height - 1 do
        for col in rect.Loc.X .. rect.Loc.X + rect.Dims.Width - 1 do
            if grid.[col, row] > 1 then
                overlapped := true

    !overlapped

let findClaim (rects: Utils.Rectangle array) grid =
    Array.fold
        (fun acc (r: Utils.Rectangle) ->
            match acc with
            | Some a -> Some a
            | None ->
                if not (overlapped r grid) then
                    Some r.Id
                else
                    None)
        None
        rects

let run exp fileName =
    let rects = Parser.parseInput fileName
    let grid = Utils.createGrid rects
    let claimOpt = findClaim rects grid

    match claimOpt with
    | None -> failwith "Claim Not Found"
    | Some claim -> Aoc.Utils.Run.checkResult exp claim
