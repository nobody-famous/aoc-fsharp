module Aoc.Year2018.Day4.Part1

open System.Collections.Generic
open Aoc.Year2018.Day4.Utils

let addMinutes (minutes: Dictionary<int, int array>) id (entry: GuardEntry) =
    if not (minutes.ContainsKey id) then
        minutes.[id] <- Array.init 60 (fun n -> 0)

    for minute in entry.Start.Minute .. entry.End.Minute - 1 do
        minutes.[id].[minute] <- minutes.[id].[minute] + 1

let tableToMinutes (table: Dictionary<int, GuardEntry list>) =
    let minutes = Dictionary<int, int array>()

    table
    |> Seq.toList
    |> Seq.iter (fun (KeyValue (id, entries)) -> List.iter (fun entry -> addMinutes minutes id entry) entries)

    minutes

let highMinute (minuteMap: Dictionary<int, int array>, id) =
    let minutes = minuteMap.[id]
    let highNdx = ref 0

    for ndx in 1 .. Array.length minutes - 1 do
        if minutes.[ndx] > minutes.[!highNdx] then
            highNdx := ndx

    (id, !highNdx)

let mostMinutes (minuteMap: Dictionary<int, int array>) =
    let id, _ =
        minuteMap
        |> Seq.toList
        |> Seq.map (fun (KeyValue (id, minutes)) -> (id, Array.sum minutes))
        |> Seq.fold
            (fun (curId, curHigh) (id, count) ->
                if count > curHigh then
                    (id, count)
                else
                    (curId, curHigh))
            (0, 0)

    (minuteMap, id)

let answer (id, minute) = id * minute

let run exp fileName =
    Parser.parseInput fileName
    |> tableToMinutes
    |> mostMinutes
    |> highMinute
    |> answer
    |> Aoc.Utils.Run.checkResult exp
