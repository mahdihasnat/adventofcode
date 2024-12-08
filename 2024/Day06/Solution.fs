module Day06

let readInput () : array<string> =
    System.IO.File.ReadLines("Day06/input.txt")
    |> Seq.toArray


let directions : array<int * int> = [|
    (-1, 0)
    (0, +1)
    (+1, 0)
    (0, -1)
|]

let rec visit (grid: array<string>) (x: int, y: int) (dir: int) (visited: Set<(int * int) * int>): Set<(int * int) * int> * (* StuckInLoop *) bool =
    if visited.Contains((x, y), dir) then
        visited, true
    else
        let visited = visited.Add((x, y), dir)
        let ox, oy = x, y
        let odir = dir
        let dx, dy = directions.[dir]
        let x, y = x + dx, y + dy
        let dir = (dir + 1) % 4
        if x < 0 || x >= grid.Length || y < 0 || y >= grid.[0].Length then
            visited, false
        elif grid.[x].[y] = '#' then
            visit grid (ox, oy) dir visited
        else
            visit grid (x, y) odir visited


let getStartingLocation (grid: array<string>) : int * int =
    grid
    |> Array.mapi (fun i arr ->
        arr.ToCharArray()
        |> Array.mapi (fun j c ->
            if c = '^' then
                Some (i, j)
            else
                None
        )
        |> Array.choose id
        |> Array.tryHead
    )
    |> Array.choose id
    |> Array.tryHead
    |> Option.get

let distinctLocation () : int =
    let grid = readInput ()
    let x, y = getStartingLocation grid
    visit grid (x, y) 0 Set.empty
    |> fst
    |> Set.map fst
    |> Set.count

let loopGenCount () : int =
    let grid = readInput ()
    let sx, sy = getStartingLocation grid
    grid
    |> Array.mapi (fun i arr ->
        arr.ToCharArray()
        |> Array.mapi (fun j c ->
            if c <> '#' && c <> '^' then
                Some (i, j)
            else
                None
        )
        |> Array.choose id
    )
    |> Array.collect id
    // |> Array.truncate 2
    |> Array.sumBy (fun (x, y) ->
        let updatedGrid =
            grid
            |> Array.mapi (fun i arr ->
                arr.ToCharArray()
                |> Array.mapi (fun j c ->
                    if i = x && j = y then
                        '#'
                    else
                        c
                )
                |> Array.map string
                |> String.concat ""
            )
        visit updatedGrid (sx, sy) 0 Set.empty
        |> snd
        |> function
            | true -> 1
            | false -> 0
    )