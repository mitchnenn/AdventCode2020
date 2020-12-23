namespace AdventCode2020

open FsUnit
open Xunit

module TryItTests =
    [<Fact>]
    let ``Test list indexed`` () =
        let list = [1;2;3;4;5;6]
        let indexed = list |> List.indexed
        Assert.True(true)

