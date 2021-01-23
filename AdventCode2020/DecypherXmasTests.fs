namespace AdventCode2020

open System.IO
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module DecypherXmasTests =
    type DecypherXmasTestsType(output:ITestOutputHelper) =
        
        let allPossibleValues (input:int64 list) =
            let rec loop left acc =
                match left with
                | [] -> acc |> List.distinct |> List.sort
                | x::xs -> loop xs ((xs |> List.map (fun i -> x + i)) @ acc)
            loop input []
        
        [<Fact>]
        let ``Test one through 25 twenty gone first`` () =
            let firstTwentyFive = [1..19] |> Seq.append [21..25] |> Seq.append[45] |> Seq.toList |> List.map (int64)
            let allPossible26Num = allPossibleValues firstTwentyFive
            output.WriteLine(sprintf "%A" allPossible26Num)
            allPossibleValues |> should not' (contain 65L)

        let findBadXmasValue preamble (input:int64 list) =
            let rec loop (left:int64 list) =
                match (left |> List.length) >= preamble + 1 with
                | false -> -1 |> int64
                | true ->
                    let currentPreamble = left |> List.take preamble
                    let currentValue = left |> List.skip preamble |> List.take 1 |> List.head
                    let allPossibleSums = allPossibleValues currentPreamble
                    if (allPossibleSums |> List.contains currentValue) then
                        loop (left |> List.tail)
                    else
                        currentValue
            loop input        
        
        [<Fact>]
        let ``Test example and find 127`` () =
            let example5 = [35;20;15;25;47;40;62;55;65;95;102;117;150;182;127;219;299;277;309;576] |> List.map (int64)
            let preamble = 5
            let firstBadValue = findBadXmasValue preamble example5
            firstBadValue |> should equal 127L
            
        [<Fact>]
        let ``Test input with 25 preamble`` () =
            let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day9", "input.txt")
            let input = File.ReadLines(path) |> Seq.map (int64) |> Seq.toList
            let preamble = 25
            let firstBadValue = findBadXmasValue preamble input
            firstBadValue |> should equal 18272118L
