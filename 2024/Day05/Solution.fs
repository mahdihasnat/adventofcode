module Day05

open System.IO

let readInput () : array<int * int> * array<array<int>> =
    let lines = File.ReadLines("Day05/input.txt")
    seq {
        for line in lines do
            if line.Contains "|" then
                let parts = line.Split "|"
                yield Choice1Of2 (int parts[0], int parts[1]) |> Some
            elif line = "" then
                yield None
            else
                yield Choice2Of2 (Array.map (fun x -> int x) (line.Split ",")) |> Some
    }
    |> Seq.toArray
    |> Array.choose id
    |> fun arr ->
        arr |> Array.choose (fun x -> match x with Choice1Of2 x -> Some x | _ -> None),
        arr |> Array.choose (fun x -> match x with Choice2Of2 x -> Some x | _ -> None)

let sumOfValidMid () : int =
    let constraints, arr = readInput ()
    let deps =
        constraints
        |> Array.map (fun (x,y) -> y, x)
        |> Array.groupBy fst
        |> Array.map (fun (y, xs) -> y, xs |> Array.map snd |> Set.ofArray)
        |> Map.ofArray
    arr
    |> Array.choose (fun arr ->
        arr
        |> Array.mapFold (fun acc elem ->
            let dep = deps |> Map.tryFind elem |>  Option.defaultValue Set.empty
            if Set.difference acc dep |> Set.isEmpty then
                true, acc |> Set.add elem
            else
                false, acc |> Set.add elem
        ) Set.empty
        |> fst
        |> Array.forall id
        |> function
            | true -> Some arr
            | false -> None
    )
    |> Array.sumBy (fun arr -> arr[arr.Length / 2])