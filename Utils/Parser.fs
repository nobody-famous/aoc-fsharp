module utils.parser

open System.IO

let readLines fileName = File.ReadAllLines fileName

let grabFirst (lines: string array) = lines.[0]
