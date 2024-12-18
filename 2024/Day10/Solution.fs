module Day10

let readInput () : array<string> =
    System.IO.File.ReadAllLines("Day10/input.txt")
    |> Seq.toArray

let rec reachingPositions (grid: array<string>) (targetNum: int) (x: int) (y: int) : Set<int * int> =
    if x < 0 || grid.Length <= x then
        Set.empty
    elif y < 0 || grid[0].Length <= y then
        Set.empty
    else
        let digit = int (grid[x][y]) - int '0'
        if digit <> targetNum then
            Set.empty
        elif digit = 9 then
            Set.singleton (x, y)
        else
            (reachingPositions grid (targetNum + 1) (x + 1) y)
            |> (+) (reachingPositions grid (targetNum + 1) (x - 1) y)
            |> (+) (reachingPositions grid (targetNum + 1) x (y - 1))
            |> (+) (reachingPositions grid (targetNum + 1) x (y + 1))

let rec countPaths (grid: array<string>) (targetNum: int) (x: int) (y: int) : int =
    if x < 0 || grid.Length <= x then
        0
    elif y < 0 || grid[0].Length <= y then
        0
    else
        let digit = int (grid[x][y]) - int '0'
        if digit <> targetNum then
            0
        elif digit = 9 then
            1
        else
            (countPaths grid (targetNum + 1) (x + 1) y)
            |> (+) (countPaths grid (targetNum + 1) (x - 1) y)
            |> (+) (countPaths grid (targetNum + 1) x (y - 1))
            |> (+) (countPaths grid (targetNum + 1) x (y + 1))


let sumOfScores () : int =
    let grid = readInput ()
    (Array.init grid.Length id, Array.init grid[0].Length id)
    ||> Array.allPairs
    |> Array.map (fun (i, j) ->
        reachingPositions grid 0 i j
        |> Set.count
    )
    |> Array.sum

let sumOfRatings () : int =
    let grid = readInput ()
    (Array.init grid.Length id, Array.init grid[0].Length id)
    ||> Array.allPairs
    |> Array.map (fun (i, j) ->
        countPaths grid 0 i j
    )
    |> Array.sum