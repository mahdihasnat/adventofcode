module Day04

open System.IO

let readdInput () : array<string> =
    let reader = new StreamReader("Day04/input.txt")
    seq {
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }
    |> Array.ofSeq

let allDirections : array< array<int*int> > =
    seq {
        yield Seq.replicate 3 (0, 1)
        yield Seq.replicate 3 (0, -1)
        yield Seq.replicate 3 (1, 0)
        yield Seq.replicate 3 (-1, 0)
        yield Seq.replicate 3 (-1, -1)
        yield Seq.replicate 3 (-1, +1)
        yield Seq.replicate 3 (+1, -1)
        yield Seq.replicate 3 (1, 1)
    }
    |> Seq.map Array.ofSeq
    |> Seq.map (fun ops -> Array.append [| (0, 0)|] ops)
    |> Array.ofSeq


let matchString (grid: array<string>) (directions: array<int*int>) (x: int) (y: int) : bool =
    ("XMAS".ToCharArray (), directions)
    ||> Array.zip
    |> Array.mapFold (fun (x, y) (c, (dx, dy)) ->
        let x = x + dx
        let y = y + dy
        if 0 <= x && x < grid.Length && 0 <= y && y < grid[0].Length && grid[x][y] = c then
            true, (x, y)
        else
            false, (x, y)
    ) (x,y)
    |> fst
    |> Array.forall id

let countOccurrence (): int =
    let input = readdInput ()
    Seq.init input.Length id
    |> Seq.allPairs (Seq.init input[0].Length id)
    |> Seq.sumBy (fun (x,y) ->
        allDirections
        |> Array.sumBy (fun dirs ->
            matchString input dirs x y
            |> function
                | true -> 1
                | false -> 0
        )
    )
