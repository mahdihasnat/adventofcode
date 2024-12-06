module Day02

open System.IO


let readInputs () : array<array<int>> =
    let reader = File.OpenText ("Day02/input.txt")
    seq{
        while not reader.EndOfStream do
            let line = reader.ReadLine ()
            let parts = line.Split(" ");
            yield
                parts
                |> Array.map int
    }
    |> Seq.toArray

let isSafe (arr: array<int>) : bool =
    arr
    |> Array.pairwise
    |> Array.map (fun (a, b) ->
        let diff = abs (a - b)
        1 <= diff && diff <= 3, a < b
    )
    |> fun arr ->
        (arr |> Array.map fst |> Array.forall id)
        && (arr |> Array.map snd |> Set.ofArray |> Set.count = 1)

let countSafe () : int =
    let inputs = readInputs ()
    inputs
    |> Array.sumBy (fun arr ->
        arr
        |> isSafe
        |> function
            | true -> 1
            | false -> 0
    )

let countSafeWithMod () : int =
    let inputs = readInputs ()
    inputs
    |> Array.sumBy (fun arr ->
        ((arr |> isSafe) || (Seq.init arr.Length id |> Seq.map (fun i -> arr |> Array.removeAt i |> isSafe) |> Seq.exists id))
        |> function
            | true -> 1
            | false -> 0
    )