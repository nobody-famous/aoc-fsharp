module aoc.year2019.day2.part2

open System.Threading.Tasks
open aoc.year2019.intcode

type MachTask =
    { noun: int
      verb: int
      task: Task<int> }

let runCombo prog noun verb =
    prog
    |> newMachine
    |> setAddr 1 noun
    |> setAddr 2 verb
    |> utils.runMachine
    |> getAddr 0

let getVerbTasks prog noun =
    Array.init
        100
        (fun verb ->
            { noun = noun
              verb = verb
              task =
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
                if entry.task.Result = target then
                    Some(entry.noun, entry.verb)
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
