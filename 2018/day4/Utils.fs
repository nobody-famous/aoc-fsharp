module Aoc.Year2018.Day4.Utils

type DateEntry =
    { Year: int
      Month: int
      Day: int
      Hour: int
      Minute: int }

type Entry =
    | StartShift of DateEntry * int
    | Sleep of DateEntry
    | Wake of DateEntry
