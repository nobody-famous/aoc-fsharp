module Aoc.Year2018.Day10.Part1

open System.Collections.Generic
open Aoc.Year2018.Day10.Utils
open Aoc.Utils.Geometry

let findBounds points =
    points
    |> Array.fold
        (fun (minPt, maxPt) pt ->
            ({ X = (System.Math.Min(minPt.X, pt.Pos.X))
               Y = System.Math.Min(minPt.Y, pt.Pos.Y) },
             { X = System.Math.Max(maxPt.X, pt.Pos.X)
               Y = System.Math.Max(maxPt.Y, pt.Pos.Y) }))
        ({ X = System.Int32.MaxValue
           Y = System.Int32.MaxValue },
         { X = System.Int32.MinValue
           Y = System.Int32.MinValue })

let run exp fileName =
    let points = Parser.parseInput fileName
    let mutable width = System.Int32.MaxValue
    let mutable height = System.Int32.MaxValue

    for count in 1 .. 10070 do
        let (minPt, maxPt) = findBounds points
        let newWidth = maxPt.X - minPt.X + 1
        let newHeight = maxPt.Y - minPt.Y + 1

        if newWidth > width || newHeight > height then
            printfn $"Grew at {count} {width}x{height}"

        width <- newWidth
        height <- newHeight

        for ndx in 0 .. Array.length points - 1 do
            let pt = points.[ndx]

            points.[ndx] <-
                { points.[ndx] with
                      Pos =
                          { X = pt.Pos.X + pt.Vel.X
                            Y = pt.Pos.Y + pt.Vel.Y } }
