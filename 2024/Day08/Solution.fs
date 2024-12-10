module Day08

let readInput () : array<string> =
    System.IO.File.ReadAllLines("Day08/input.txt")
    |> Seq.toArray


let uniAntiNodes () : int =
    let grid = readInput ()
    grid
    |> Array.mapi (fun x row ->
        row.ToCharArray ()
        |> Array.mapi (fun y c ->
            if c <> '.' then
                Some (c, (x,y))
            else
                None
        )
        |> Array.choose id
    )
    |> Array.collect id
    |> Array.groupBy fst
    |> Array.collect (fun (c, arr) ->
        arr
        |> Array.map snd
        |> fun x -> (x, x)
        ||> Array.allPairs
        |> Array.choose (fun ((x1, y1), (x2, y2)) ->
            if x1 = x2 && y1 = y2 then
                None
            else
                let dx = x2 - x1
                let dy = y2 - y1
                let x = x2 + dx
                let y = y2 + dy
                if x < 0 || y < 0 || x >= grid.Length || y >= grid.[0].Length then
                    None
                else
                    Some (x, y)
        )
    )
    |> Set.ofArray
    |> fun pos ->
        pos
        |> Set.iter (printfn "%A")
        pos
    |> Set.count
