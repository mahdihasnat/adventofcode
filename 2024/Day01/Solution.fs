module Day01

open System
open System.IO
open System.Text

let readInput () : array<int * int> =
    let reader = File.OpenText ("Day01/input.txt")
    seq{
        while not reader.EndOfStream do
            let line = reader.ReadLine ()
            let parts = line.Split("   ");
            let num1 = int parts[0]
            let num2 = int parts[1]
            yield (num1, num2)
    }
    |> Seq.toArray


let sumOfSortedAbsDiff () : int  =
    let nums = readInput ()
    let num1 =  nums |> Array.map fst |> Array.sort
    let num2 =  nums |> Array.map snd |> Array.sort
    (num1, num2)
    ||> Array.zip
    |> Array.map (fun (x, y) -> abs (x - y))
    |> Array.sum

let similarityScore () : int64 =
    let nums = readInput ()
    let num1 =  nums |> Array.map fst |> Array.sort
    let num2 =  nums |> Array.map snd |> Array.sort
    let counts =
        num2
        |> Array.countBy id
        |> Map.ofArray
    num1
    |> Array.map (fun n ->
        counts.TryFind n
        |> function
            | None -> 0L
            | Some x -> ((int64) n) * (int64) x
        )
    |> Array.sum

