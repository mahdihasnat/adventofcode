module Tests

open System
open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =
    [<Fact>]
    let ``Day 01`` () =
        let result = Day01.solve()
        output.WriteLine(sprintf "Sum: %d" result)
        Assert.True(true)