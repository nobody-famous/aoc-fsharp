module Utils.Parser

open System.IO

let readLines fileName = File.ReadAllLines fileName

let grabFirst (lines: string array) = lines.[0]
