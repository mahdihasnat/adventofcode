module Day07


let readInput () : array<int64 * array<int64>> =
    System.IO.File.ReadLines("Day07/input.txt")
    |> Seq.map (fun line ->
        let parts = line.Split(':')
        let sum = int64 parts[0]
        let nums =
            parts[1].Split(' ')
            |> Array.filter (fun s -> s.Length > 0)
            |> Array.map int64
        sum, nums
    )
    |> Seq.toArray

let genPossibleSums (arr: array<int64>) : seq<int64> =
    let n = arr.Length
    let pos = n - 1
    Seq.init (1 <<< pos) id
    |> Seq.map (fun mask ->
        ([| 0 .. pos - 1 |], arr |> Array.skip 1)
        ||> Array.zip
        |> Array.fold (fun acc (i, elem) ->
            if (mask &&& (1 <<< i)) = 0 then
                acc + elem
            else
                acc * elem
        ) arr[0]
    )

let concat (x: int64) (y: int64) : int64 =
    let x = x.ToString()
    let y = y.ToString()
    int64 (x + y)


let genPossible3Sum (arr: array<int64>) : seq<int64> =
    let n = arr.Length
    let pos = n - 1
    (Seq.singleton [||], Array.init pos id)
    ||> Array.fold (fun sq i ->
        sq
        |> Seq.collect (fun arr ->
            seq {
                yield Array.append [| 0 |] arr
                yield Array.append [| 1 |] arr
                yield Array.append [| 2 |] arr
            }
        )
    )
    |> Seq.map (fun ops ->
        (ops, arr |> Array.skip 1)
        ||> Array.zip
        |> Array.fold (fun acc (t, elem) ->
            if t = 0 then
                acc + elem
            elif t = 1 then
                acc * elem
            else
                concat acc elem
        ) arr.[0]
    )


let sumOfTrueSum () : int64 =
    let inputs = readInput ()
    inputs
    |> Array.filter (fun (sum, arr) ->
        genPossibleSums arr
        |> Seq.exists (fun s -> s = sum)
    )
    |> Array.sumBy fst

let sumOfTrue3Sum () : int64 =
    let inputs = readInput ()
    inputs
    |> Array.filter (fun (sum, arr) ->
        genPossible3Sum arr
        |> Seq.exists (fun s -> s = sum)
    )
    |> Array.sumBy fst
