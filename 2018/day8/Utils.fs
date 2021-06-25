module Aoc.Year2018.Day8.Utils

type Node = { Kids: Node list; Metadata: int list }

let parseTree input =
    let rec parseNode numbers =
        match numbers with
        | kidCount :: metaCount :: body ->
            let mutable kids = []
            let mutable meta = []
            let mutable rest = body

            for _ in 1 .. kidCount do
                let (kid, rem) = parseNode rest
                kids <- kid :: kids
                rest <- rem

            for _ in 1 .. metaCount do
                meta <- (List.head rest) :: meta
                rest <- List.tail rest

            ({ Kids = kids; Metadata = meta }, rest)

        | n -> failwith $"Invalid node {n}"

    parseNode input |> fst
