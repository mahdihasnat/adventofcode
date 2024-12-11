module Day09

let readInput () : string =
    System.IO.File.ReadAllLines "Day09/input.txt"
    |> Seq.toArray
    |> Array.exactlyOne

[<RequireQualifiedAccess>]
type BlockType =
    | Empty
    | Data of Id: int

let fileBlocks (repr: string) : array<BlockType> =
    repr.ToCharArray ()
    |> Array.mapFold (fun (nextId, isFile) (c: char) ->
        let count: int = (int c) - (int '0')
        let blocks = Array.replicate count (if isFile then BlockType.Data nextId else BlockType.Empty)
        if isFile then
            blocks, (nextId + 1, false)
        else
            blocks, (nextId, true)
    ) (0, true)
    |> fst
    |> Array.collect id

let fileBlockIds (blocks: array<BlockType>) : array<int> =
    let totalFileBlocks =
        blocks
        |> Array.filter (fun x -> match x with | BlockType.Data _ -> true | _ -> false)
        |> Array.length
    let revBlocks =
        blocks
        |> Array.rev
        |> Array.choose (function | BlockType.Data id -> Some id | BlockType.Empty -> None)
        |> Array.toList

    blocks
    |> Array.mapFold (fun (remainingBlocks: list<int>) block ->
        match block with
        | BlockType.Data id ->
            BlockType.Data id, remainingBlocks
        | BlockType.Empty ->
            BlockType.Data remainingBlocks.Head, remainingBlocks.Tail
    ) (revBlocks)
    |> fst
    |> Array.take totalFileBlocks
    |> Array.choose (function | BlockType.Data id -> Some id | _ -> None)



let checkSum () : int64 =
    let input = readInput ()
    input
    |> fileBlocks
    |> fileBlockIds
    |> Array.mapi (fun i x -> int64 i * int64 x)
    |> Array.sum