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