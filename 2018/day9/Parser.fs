module Aoc.Year2018.Day9.Parser

open Aoc.Year2018.Day9.Utils
open Aoc.Utils.Regex

let regex =
    @"([0-9]+) players; last marble is worth ([0-9]+) points"

let parseInput fileName =
    let lines = Aoc.Utils.Parser.readLines fileName

    match lines.[0] with
    | MatchPattern regex [ numPlayers; lastMarble ] ->
        { Players = int numPlayers
          LastMarble = int lastMarble }
    | line -> failwith $"Invalid input: {line}"
