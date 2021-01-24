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
        
        let example5 = [35;20;15;25;47;40;62;55;65;95;102;117;150;182;127;219;299;277;309;576] |> List.map (int64)
        
        [<Fact>]
        let ``Test example and find 127`` () =
            let preamble = 5
            let firstBadValue = findBadXmasValue preamble example5
            output.WriteLine(sprintf "%i" firstBadValue)
            firstBadValue |> should equal 127L
        
        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day9", "input.txt")
        let input = File.ReadLines(path) |> Seq.map (int64) |> Seq.toList
        
        [<Fact>]
        let ``Test input with 25 preamble`` () =
            let preamble = 25
            let firstBadValue = findBadXmasValue preamble input
            firstBadValue |> should equal 18272118L

        let sumUntilValue (input:int64 list) (value:int64) =
            let rec loop left members =
                match left with
                | [] -> None
                | _ ->
                    let current = left |> List.head
                    let tempSum = (members |> List.sum) + current
                    if tempSum = value then
                        Some (current::members |> List.rev)
                    elif tempSum > value then
                        None
                    else
                        loop (left |> List.tail) (current::members) 
            loop input []
        
        [<Fact>]
        let ``Test sum until a value fail`` () =
            let members = sumUntilValue (example5 |> List.take 10) 127L
            members |> should be Null
            
        [<Fact>]
        let ``Test sum until a value succeed`` () =
            let members = sumUntilValue (example5 |> List.skip 2 |> List.take 10) 127L
            members |> should not' Null
            members.Value |> should equal [15L;25L;47L;40L]
            
        let findFirstSum (input:int64 list) (value:int64) =
            let rec loop left = 
                match left with
                | [] -> []
                | _ ->
                    match sumUntilValue left value with
                    | Some(members) -> members
                    | None -> loop (left |> List.tail) 
            loop input
            
        [<Fact>]
        let ``Test find first sum`` () =
            let members = findFirstSum example5 127L
            members |> should not' Null
            members |> should equal [15L;25L;47L;40L]

        [<Fact>]
        let ``Test input for first sum`` () =
            let members = findFirstSum input 18272118L
            members |> should equal [720149L; 731033L; 1466212L; 1388501L; 1349179L; 816228L; 923226L; 973294L; 1004417L; 1122441L; 948607L; 979730L; 1398951L; 1038383L; 1079070L; 1116758L; 1215939L]
            let sum = (members |> List.min) + (members |> List.max)
            sum |> should equal 2186361L
