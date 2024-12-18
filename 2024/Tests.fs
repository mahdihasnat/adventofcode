module Tests

open System
open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =
    [<Fact>]
    let ``Day 01`` () =
        output.WriteLine($"SumOfSortedAbsDiff: {Day01.sumOfSortedAbsDiff ()}")
        output.WriteLine($"SimilarityScore: {Day01.similarityScore ()}")
        Assert.True(true)

    [<Fact>]
    let ``Day 02`` () =
        output.WriteLine($"Count: {Day02.countSafe ()}");
        output.WriteLine($"CountWithMod: {Day02.countSafeWithMod ()}")

    [<Fact>]
    let ``Day 03`` () =
        output.WriteLine($"SumOfMul: {Day03.sumOfMul ()}");
        output.WriteLine($"ConditionalSumOfMul: {Day03.conditionalSumOfMul ()}")

    [<Fact>]
    let ``Day 04`` () =
        output.WriteLine($"CountOccurrence: {Day04.countOccurrence ()}")
        output.WriteLine($"CountCrossOccurrence: {Day04.countCrossOccurrence ()}")

    [<Fact>]
    let ``Day 05`` () =
        output.WriteLine($"SumOfValidMid: {Day05.sumOfValidMid ()}")
        output.WriteLine($"SumOfInvalidMid: {Day05.sumOfInvalidMid ()}")

    [<Fact>]
    let ``Day 06`` () =
        output.WriteLine($"DistinctLocation: {Day06.distinctLocation ()}")
        output.WriteLine($"LoopGenCount: {Day06.loopGenCount ()}")

    [<Fact>]
    let ``Day 07`` () =
        output.WriteLine($"SumOfTrueSum: {Day07.sumOfTrueSum ()}")
        output.WriteLine($"SumOfTrue3Sum: {Day07.sumOfTrue3Sum ()}")

    [<Fact>]
    let ``Day 08`` () =
        output.WriteLine($"UniAntiNodes: {Day08.uniAntiNodes ()}")
        output.WriteLine($"UniqueMultipleAntiNodes: {Day08.uniqueMultipleAntiNodes ()}")

    [<Fact>]
    let ``Day 09`` () =
        output.WriteLine($"CheckSum: {Day09.checkSum ()}")
        output.WriteLine($"CgeckSumWithCompleteMove: {Day09.checkSumWithCompleteMove ()}")

    [<Fact>]
    let ``Day 10`` () =
        output.WriteLine ($"SumOfScores: {Day10.sumOfScores ()}")