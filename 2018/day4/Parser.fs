module Aoc.Year2018.Day4.Parser

open System.Text.RegularExpressions
open Aoc.Year2018.Day4.Utils

let dateRegex =
    @"\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\]"

let shiftRegex = @"Guard #([0-9]+) begins shift"
let sleepRegex = @"falls asleep"
let wakeRegex = @"wakes up"

let startShiftRegex = $"{dateRegex} {shiftRegex}"
let fallAsleepRegex = $"{dateRegex} {sleepRegex}"
let wakeUpRegex = $"{dateRegex} {wakeRegex}"

let (|LogEntry|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let getEntry line =
    match line with
    | LogEntry startShiftRegex [ year; month; day; hour; minute; guard ] ->
        StartShift(
            { Year = int year
              Month = int month
              Day = int day
              Hour = int hour
              Minute = int minute },
            int guard
        )
    | LogEntry fallAsleepRegex [ year; month; day; hour; minute ] ->
        Sleep(
            { Year = int year
              Month = int month
              Day = int day
              Hour = int hour
              Minute = int minute }
        )
    | LogEntry wakeUpRegex [ year; month; day; hour; minute ] ->
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

let parseInput fileName =
    Aoc.Utils.Parser.readLines fileName
    |> Array.map getEntry
    |> Array.sortWith compareEntries
