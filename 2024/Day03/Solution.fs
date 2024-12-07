module Day03


open System.IO
open System.Text.RegularExpressions;

let readInput () : string =
    File.ReadAllText ("Day03/input.txt")

let sumOfMul () : int =
    let input = readInput ()
    let pattern = @"mul\(([0-9]{1,3}),([0-9]{1,3})\)"
    let regex = Regex(pattern)
    let matches = regex.Matches(input)
    matches
    |> Seq.map (fun m -> int m.Groups[1].Value, int m.Groups[2].Value)
    |> Seq.sumBy (fun (x, y) -> x * y)

let conditionalSumOfMul () : int =
    let input = readInput ()
    // let input = "do()"
    let pattern =
        Regex.Escape("mul(") + "([0-9]{1,3}),([0-9]{1,3})" + Regex.Escape(")")
        + "|" + Regex.Escape("do()")
        + "|" + Regex.Escape("don't()")

    let regex = Regex(pattern)
    let matches = regex.Matches(input)
    matches
    |> Seq.fold (fun (enabled, sum) m ->
        if  m.Value = "do()" then
            (true, sum)
        elif m.Value = "don't()" then
            (false, sum)
        elif enabled then
            (enabled, sum + int m.Groups[1].Value * int m.Groups[2].Value)
        else
            (enabled, sum)
    ) (true, 0)
    |> snd