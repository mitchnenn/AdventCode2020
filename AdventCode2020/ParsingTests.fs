namespace AdventCode2020

open FSharp.Text.RegexProvider
open FParsec
open FParsecHelper
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

type KVP = {key:string; value:string}
type KVPRegex = Regex < @"^(?<Key>.*):(?<Value>.*)$" >

module ParsingTests =
    type ParsingTestsType(output:ITestOutputHelper) =
        
        [<Fact>]
        let ``FsUnit test`` () =
            1 |> should equal 1
            1 |> should not' (equal 2)
            test output pfloat "1.25" 
        
        let getMin : Parser<_> = pint32 .>> pchar '-'
        let getMax : Parser<_> = pint32 .>> spaces
        let getRange : Parser<_> = getMin .>>. getMax  

        [<Fact>]
        let ``Parse min max`` () =
            let input = "7-16 r: nvrrzhsgrwthgbrqkqh"
            test output getMin input
            test output (tuple2 getMin getMax) input
            test output getRange input
            Assert.True(true)

        let passports = @"hgt:176cm
                iyr:2013
                hcl:#fffffd ecl:amb
                byr:2000
                eyr:2034
                cid:89 pid:934693255
                
                hcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017
                byr:1939
                eyr:2020
                
                pid:526669252 eyr:1972
                hgt:152cm ecl:dne byr:1960 hcl:z iyr:2023"
        
        let fieldParser = regex @".*:.*" .>> spaces
        
        let parseField input =
            match run fieldParser input with
            | Success(result,_,_) -> result.Trim()
            | Failure _ -> failwith "Invalid filed input."
            
        let parseLine (input:string) =
            input.Split(" ")
            |> Seq.map(parseField)
            |> Seq.toList
        
        [<Fact>]
        let ``Field Parser Test`` () =
            let line = "hcl:#fffffd ecl:amb\n"
            let actual = parseLine line
            let expected = ["hcl:#fffffd"; "ecl:amb"]
            actual |> should equal expected
        
        let parsePassports (lines:string seq) =
            let rec loop lines current passports = 
                match lines with
                | [] -> current::passports |> List.rev
                | x :: xs ->
                    match x with
                    | "" -> loop xs [] (current::passports)
                    | _ ->
                        let fields = parseLine x
                        loop xs (current @ fields) passports
            loop (lines |> Seq.toList) [] []
        
        [<Fact>]
        let ``Parse passports test`` () =
            let lines = passports.Split("\n")
                        |> Seq.map(fun i -> i.Trim())
                        |> Seq.toList
            let allPassports = parsePassports lines                      
            let expected =  ["hgt:176cm"; "iyr:2013"; "hcl:#fffffd"; "ecl:amb"; "byr:2000"; "eyr:2034"; "cid:89"; "pid:934693255"]
            let actual = allPassports.[0] 
            actual |> should equal expected
        
        let convertKeyValuePair (input:string) =
            let m = KVPRegex().TypedMatch(input)
            { key=m.Key.Value; value=m.Value.Value }
        
        [<Fact>]
        let ``Split field test`` () =
            let input = ["cid:170"; "hcl:#b6652a"; "byr:2011"; "ecl:gry"; "iyr:2025"; "pid:#b6e567"; "hgt:67cm"; "eyr:2016"]
            let keyValuePairs = input |> List.map(convertKeyValuePair)
            keyValuePairs |> should equal []
