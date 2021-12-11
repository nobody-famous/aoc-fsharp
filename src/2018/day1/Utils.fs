module y2018.day1.utils

let parse (input: string) =
    input.Split '\n'
    |> Array.filter (fun s -> s.Trim().Length > 0)
    |> Array.map (fun s -> int s)
    |> Array.toList
