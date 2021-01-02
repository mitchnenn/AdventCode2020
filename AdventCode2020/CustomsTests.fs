namespace AdventCode2020

open System.IO
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module CustomsTests =
    type CustomsTestsType(output:ITestOutputHelper) =
        let testInput = ["abc";"";"a";"b";"c";"";"ab";"ac";"";"a";"a";"a";"a";"";"b"]
        
        let parseGroups (lines:string list) =
            let rec loop linesLeft currentGroup groups =
                match linesLeft with
                | [] ->
                    match currentGroup with
                    | [] -> groups |> List.rev
                    | _ -> currentGroup::groups |> List.rev
                | x::xs ->
                    match x with
                    | "" -> loop xs [] (currentGroup::groups)
                    | _ -> loop xs (x::currentGroup) groups
            loop lines [] []
        
        [<Fact>]
        let ``Parse groups test`` () =
            let groups = parseGroups testInput
            groups |> List.length |> should equal 5
            groups |> List.head |> List.length |> should equal 1
            groups |> List.item 1 |> List.length |> should equal 3

        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day6", "input.txt")
        
        let parseAllGroups : string list list =
            let lines = File.ReadAllLines(path)
                        |> Seq.toList
            let groups = parseGroups lines
            groups
        
        [<Fact>]
        let ``Parse input text`` () =
            let allGroups = parseAllGroups
            allGroups |> List.length |> should equal 462
            
        let sumYesQuestionsForGroup (persons:string list) =
            let rec loop (personsLeft:string list) acc =
                match personsLeft with
                | [] -> acc
                | x::xs -> loop xs (acc @ (x.ToCharArray() |> Array.toList))
            loop persons [] 
            
        [<Fact>]
        let ``Test sum yes questions for a group`` () =
            let input = ["abc";"a";"abcdefg"]
            let actualSum = sumYesQuestionsForGroup input
            output.WriteLine($"{actualSum}")
            actualSum
            |> List.distinct
            |> List.length
            |> should equal 7
        
        let sumYesQuestionsForAllGroups (groups:string list list) =
            let rec loop groupsLeft acc =
                match groupsLeft with
                | [] -> acc
                | x::xs ->
                    let count = sumYesQuestionsForGroup x
                                |> List.distinct
                                |> List.length
                    loop xs (acc + count)
            loop groups 0
        
        [<Fact>]
        let ``Test sum yes questions a couple of groups`` () =
            let input = [["abc";"a";"abcdefg"];["a";"bcde";"a"]]
            let actualSum = sumYesQuestionsForAllGroups input
            output.WriteLine($"{actualSum}")
            actualSum |> should equal 12
        
        [<Fact>]
        let ``Sum yes votes by group`` () =
            let allGroups = parseAllGroups
            let sum = sumYesQuestionsForAllGroups allGroups
            output.WriteLine($"{sum}")
            sum |> should equal 6249