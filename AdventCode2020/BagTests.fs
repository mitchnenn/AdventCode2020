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

        let example = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.".Split(Environment.NewLine)
        
        let applies bag (rule:Rule) =
            snd rule
            |> List.map snd
            |> List.contains bag
                       
        let rec findBagsThatContainBag rulebook bag =
           let containerBags = rulebook
                               |> Seq.filter (applies bag)
                               |> Seq.map fst
                               |> Seq.toList
           containerBags @ (containerBags |> List.collect (findBagsThatContainBag rulebook))
                    
        [<Fact>]
        let ``Shiny gold on example input`` () =
            let rulebook = example |> Seq.map parseRule |> List.ofSeq
            let count = findBagsThatContainBag rulebook "shiny gold" |> List.distinct |> List.length
            output.WriteLine(sprintf "%d" count)
            count |> should equal 4
        
        [<Fact>]
        let ``Shiny bag rules from input`` () =
            let rulebook = input |> Seq.map parseRule |> List.ofSeq
            let count = findBagsThatContainBag rulebook "shiny gold" |> List.distinct |> List.length
            output.WriteLine(sprintf "%d" count)
            count |> should equal 211
        
        let findBagRule rulebook bag =
            rulebook |> List.find (fun r -> fst r = bag)

        let rec numOfBagsInBag (rulebook:Rule list) bag =
            let bagRule = findBagRule rulebook bag
            let containedBags = bagRule |> snd
            containedBags |> List.sumBy (fun (nb, bag) -> nb + (nb * numOfBagsInBag rulebook bag))
                        
        [<Fact>]
        let ``Shiny gold bag contains from example`` () =
            let rulebook = example |> Seq.map parseRule |> List.ofSeq
            let count = numOfBagsInBag rulebook "shiny gold" 
            count |> should equal 32
            
        [<Fact>]
        let ``Num of bags in bag input`` () =
            let rulebook = input |> Seq.map parseRule |> List.ofSeq
            let count = numOfBagsInBag rulebook "shiny gold" 
            output.WriteLine(sprintf "%d" count)
            count |> should equal 12414
