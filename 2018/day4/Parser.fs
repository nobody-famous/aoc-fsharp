module Aoc.Year2018.Day4.Parser

open System.Collections.Generic
open Aoc.Year2018.Day4.Utils
open Aoc.Utils.Regex

let dateRegex =
    @"\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\]"

let shiftRegex = @"Guard #([0-9]+) begins shift"
let sleepRegex = @"falls asleep"
let wakeRegex = @"wakes up"

let startShiftRegex = $"{dateRegex} {shiftRegex}"
let fallAsleepRegex = $"{dateRegex} {sleepRegex}"
let wakeUpRegex = $"{dateRegex} {wakeRegex}"

let getEntry line =
    match line with
    | MatchPattern startShiftRegex [ year; month; day; hour; minute; guard ] ->
        StartShift(
            { Year = int year
              Month = int month
              Day = int day
              Hour = int hour
              Minute = int minute },
            int guard
        )
    | MatchPattern fallAsleepRegex [ year; month; day; hour; minute ] ->
        Sleep(
            { Year = int year
              Month = int month
              Day = int day
              Hour = int hour
              Minute = int minute }
        )
    | MatchPattern wakeUpRegex [ year; month; day; hour; minute ] ->
        Wake(
            { Year = int year
              Month = int month
              Day = int day
              Hour = int hour
              Minute = int minute }
        )
    | l -> failwith $"Invalid line {l}"

let getEntryDate entry =
    match entry with
    | StartShift (date, _) -> date
    | Sleep (date) -> date
    | Wake (date) -> date

let compareDates (date1: DateEntry) (date2: DateEntry) =
    if date1 = date2 then
        0
    else if date1.Year < date2.Year then
        -1
    else if date1.Year > date2.Year then
        1
    else if date1.Month < date2.Month then
        -1
    else if date1.Month > date2.Month then
        1
    else if date1.Day < date2.Day then
        -1
    else if date1.Day > date2.Day then
        1
    else if date1.Hour < date2.Hour then
        -1
    else if date1.Hour > date2.Hour then
        1
    else if date1.Minute < date2.Minute then
        -1
    else if date1.Minute > date2.Minute then
        1
    else
        0

let compareEntries entry1 entry2 =
    let date1 = getEntryDate entry1
    let date2 = getEntryDate entry2

    compareDates date1 date2

let addToTable (table: Dictionary<int, GuardEntry list>) id entry =
    if not (table.ContainsKey id) then
        table.[id] <- []

    table.[id] <- entry :: table.[id]

let buildTable (entries: Entry array) =
    let table = Dictionary<int, GuardEntry list>()

    let rec loop ndx curId curStart =
        if ndx < Array.length entries then
            match entries.[ndx] with
            | StartShift (date, id) -> loop (ndx + 1) id None
            | Sleep (date) -> loop (ndx + 1) curId (Some date)
            | Wake (date) ->
                match curStart with
                | None -> failwith "Wake with no sleep"
                | Some s -> addToTable table curId { Start = s; End = date }

                loop (ndx + 1) curId curStart

    loop 0 0 None

    table

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

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map getEntry
    |> Array.sortWith compareEntries
    |> buildTable
    |> tableToMinutes
