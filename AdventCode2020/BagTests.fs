namespace AdventCode2020

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Xunit.Abstractions
open FsUnit

 type Rule = string * (int * string) list 

module BagTests =
    type BatTestsType(output:ITestOutputHelper) =
            
        let parseRule rule : Rule =
            Regex(@"(?<Bag>.+) bags contain (?<Rules>.+)\.").Match(rule)
            |> (fun m ->
                (m.Groups.["Bag"].Value,
                 m.Groups.["Rules"].Value.Split(", ", StringSplitOptions.RemoveEmptyEntries)
                   |> Seq.except [|"no other bags"|]
                   |> Seq.map (Regex(@"(?<Count>\d+) (?<Bag>.+) bags?").Match)
                   |> Seq.map (fun m -> ((int)m.Groups.["Count"].Value, m.Groups.["Bag"].Value))
                   |> Seq.toList))
        
        [<Fact>]
        let ``Parse a rule test`` () =
            let aRule = @"light red bags contain 1 bright white bag, 2 muted yellow bags."
            let result = sprintf "%A" (parseRule aRule)
            output.WriteLine(result)
            result |> should equal "(\"light red\", [(1, \"bright white\"); (2, \"muted yellow\")])"
            
        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day7", "input.txt")
        let input = File.ReadLines(path)
        
        [<Fact>]
        let ``Parse input rules test`` () =
            let result = input |> Seq.map parseRule
            let firstRule = result |> Seq.head |> sprintf "%A"
            firstRule |> should equal "(\"shiny lime\", [(3, \"muted magenta\"); (3, \"clear cyan\")])"

        let applies bag (rule:Rule) =
            snd rule
            |> List.map snd
            |> List.contains bag
                    
        [<Fact>]
        let ``Shiny bag rules`` () =
            let rulebook = input |> Seq.map parseRule
            let myBag = "shiny gold"
            let direct = rulebook |> Seq.filter (applies myBag)
            output.WriteLine(sprintf "%A" direct)
          