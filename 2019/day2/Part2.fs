module Aoc.Year2019.Day2.Part2

open System.Threading.Tasks
open Aoc.Year2019.Intcode

type MachTask =
    { Noun: int
      Verb: int
      Task: Task<int> }

let runCombo prog noun verb =
    prog
    |> newMachine
    |> setAddr 1 noun
    |> setAddr 2 verb
    |> Utils.runMachine
    |> getAddr 0

let getVerbTasks prog noun =
    Array.init
        100
        (fun verb ->
            { Noun = noun
              Verb = verb
              Task =
                  async { return runCombo prog noun verb }
                  |> Async.StartAsTask })

let genTasks prog =
    Array.init 100 (fun noun -> getVerbTasks prog noun)

let findGroupTarget target group =
    Array.fold
        (fun acc' entry ->
            match acc' with
            | Some v -> Some v
            | None ->
                if entry.Task.Result = target then
                    Some(entry.Noun, entry.Verb)
                else
                    None)
        None
        group

let findTarget target taskGroups =
    Array.fold
        (fun acc group ->
            match acc with
            | Some v -> Some v
            | None -> findGroupTarget target group)
        None
        taskGroups

let toAnswer =
    function
    | None -> 0
    | Some (noun, verb) -> (noun * 100) + verb

let run fileName =
    parseInput fileName
    |> genTasks
    |> findTarget 19690720
    |> toAnswer
