module Day09

let readInput () : string =
    System.IO.File.ReadAllLines "Day09/input.txt"
    |> Seq.toArray
    |> Array.exactlyOne

[<RequireQualifiedAccess>]
type BlockType =
    | Empty
    | Data of Id: int

[<RequireQualifiedAccess>]
type Segment =
    | Empty of Length: int * Index: int
    | File of Length: int * Index: int * Id: int

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


let fileSegments (repr: string) : array<Segment> =
    repr.ToCharArray ()
    |> Array.mapi (fun index elem -> (index, elem))
    |> Array.mapFold (fun (nextId, isFile) (index, c: char) ->
        let count: int = (int c) - (int '0')
        if isFile then
            (Segment.File (count, index, nextId)), (nextId + 1, false)
        else
            (Segment.Empty (count, index)), (nextId, true)
    ) (0, true)
    |> fst

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

let blocksFromSegments (arr: array<Segment>) : array<BlockType> =
    arr
    |> Array.collect (function
        |Segment.Empty (len, index) ->
            Array.init len (fun _ -> BlockType.Empty)
        | Segment.File (len, index, id) ->
            Array.init len (fun _ -> BlockType.Data id)
    )

let checkSumWithCompleteMove () : int64 =
    let input = readInput ();
    // let input = "2333133121414131402"
    let segments =
        input
        |> fileSegments
    let emptySegments : array<Set<int>> =
        segments
        |> Array.choose (function
            | Segment.Empty (len, index) -> Some (len, index)
            | Segment.File _ -> None
        )
        |> fun arr ->
            Array.foldBack (fun (len, index) (arrs) ->
                    Array.updateAt len (Set.add index (arrs.[len]) ) arrs
                )
                arr (Array.init 10 (fun _ -> Set.empty))


    let segments : Segment array =
        segments
        // |> fun x ->
        //     printfn "input: %A" x
        //     printfn "emptySegments: %A" emptySegments
        //     x
        |> Array.rev
        |> Array.mapFold (fun (emptySegments : array<Set<int>>) (segment) ->
            match segment with
            | Segment.Empty _ -> None, emptySegments
            | Segment.File (length, index, id) ->
                emptySegments
                |> Array.mapi (fun index arr -> (index, arr))
                |> Array.tryPick (fun (holeLength, holeIndexes) ->
                    if length <= holeLength then
                        if holeIndexes.IsEmpty then
                            None
                        else
                            let firstPos = holeIndexes.MinimumElement
                            if firstPos < index then
                                let remainingHoleLength  = holeLength - length
                                let updatedEmptySegments =
                                    emptySegments
                                    |> fun emptySegments ->
                                        emptySegments
                                        |> Array.updateAt holeLength (Set.difference holeIndexes (Set.singleton firstPos))
                                    |> fun emptySegments ->
                                        emptySegments
                                        |> Array.updateAt remainingHoleLength (Set.add firstPos (emptySegments.[remainingHoleLength]))
                                    |> fun emptySegments ->
                                        emptySegments
                                        |> Array.updateAt length (Set.add index (emptySegments.[length]))

                                Some (firstPos, updatedEmptySegments)
                            else
                                None
                    else
                        None
                )
                |> Option.map (fun (firstPos, emptySegments) ->
                    // printfn $"Moving %A{(length, index, id)} to %A{firstPos} empty: %A{emptySegments}"
                    (Some (Segment.File (length, firstPos, id))), emptySegments
                )
                |> Option.defaultWith (fun () ->
                    // printfn $"NotMoving %A{(length, index, id)} empty: %A{emptySegments}"
                    (Some (Segment.File (length, index, id))), emptySegments
                )
        ) emptySegments
        |> fun (fileSegments, emptySegments) ->
            emptySegments
            |> Array.mapi (fun len indexes ->
                if len > 0 then
                    indexes
                else
                    Set.empty
                |> Array.ofSeq
                |> Array.map (fun index -> Segment.Empty (len, index))
            )
            |> Array.collect id
            |> Array.append (fileSegments |> Array.choose id)
            |> Array.sortBy (function | Segment.Empty (len, index) -> index, 0 | Segment.File (len, index, id) -> index, - id)

    segments
    // |> fun x ->
    //     printfn "%A" x
    //     x
    |> blocksFromSegments
    // |> fun x ->
    //     printfn "%A" x
    //     x
    // |> Array.choose (fun x -> match x with | BlockType.Data _ -> Some x | _ -> None)
    |> Array.mapi (fun i x -> int64 i * (match x with | BlockType.Data t -> int64 t | BlockType.Empty -> 0L))
    |> Array.sum
