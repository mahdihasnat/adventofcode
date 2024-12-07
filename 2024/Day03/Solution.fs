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