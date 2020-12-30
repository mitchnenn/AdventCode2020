#r @"/Users/mnenn/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsec.dll"
#r @"/Users/mnenn/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsecCS.dll"
#r @"/Users/mnenn/.nuget/packages/fsharp.text.regexprovider/2.1.0/lib/netstandard2.0/FSharp.Text.RegexProvider.dll"
#r @"/Users/mnenn/.nuget/packages/unquote/5.0.0/lib/netstandard2.0/Unquote.dll"
open Swensen.Unquote
open FParsec
open System.IO
open FSharp.Text.RegexProvider

let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = File.ReadAllLines(path)

let fieldParser = regex @".*:.*" .>> spaces

let parseField input =
    match run fieldParser input with
    | Success(result,_,_) -> result.Trim()
    | Failure _ -> failwith "Invalid filed input."
    
let parseLine (input:string) =
    input.Split(" ")
    |> Seq.map(parseField)
    |> Seq.toList

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

let allPassports = parsePassports input

allPassports |> List.length

let required = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let isValidPassport (passport:string list) =
    required
    |> List.forall(fun r -> passport |> List.exists(fun i -> i.StartsWith(r)))

allPassports
|> List.countBy(isValidPassport)

let requiredFieldPassports = allPassports
                             |> List.filter(isValidPassport)

type KVP = {key:string; value:string}
type KVPRegex = Regex< @"^(?<Key>.*):(?<Value>.*)$" >

let convertKeyValuePair (input:string) =
    let m = KVPRegex().TypedMatch(input)
    { key=m.Key.Value; value=m.Value.Value }

let convertPassportToKVP (passport:string list) =
    passport
    |> List.map(convertKeyValuePair)

let kvpPassports = requiredFieldPassports
                   |> List.map(convertPassportToKVP)

let isYearBetween (year:string) min max =
    match Regex.IsMatch(year, @"^\d{4}$") with
    | false -> false
    | true ->
        let intYear = int year
        intYear >= min && intYear <= max

let isValidHeight height =
    let aMatch = Regex.Match(height, @"^(?<intHeight>\d+)(?<uint>in|cm)$")
    match aMatch.Success with
    | false -> false
    | true ->
        let intHeight = int aMatch.Groups.["intHeight"].Value
        match aMatch.Groups.["uint"].Value with
        | "cm" -> intHeight >= 150 && intHeight <= 193
        | "in" -> intHeight >= 59 && intHeight <= 76
        | _ -> false

let isValidField (field:KVP) =
    match field.key with
    | "byr" -> isYearBetween field.value 1920 2002
    | "iyr" -> isYearBetween field.value 2010 2020
    | "eyr" -> isYearBetween field.value 2020 2030
    | "hgt" -> isValidHeight field.value
    | "hcl" -> Regex.IsMatch(field.value, @"^#(\d|[a-f]){6}$")
    | "ecl" -> Regex.IsMatch(field.value, @"^(amb|blu|brn|gry|grn|hzl|oth)$")
    | "pid" -> Regex.IsMatch(field.value, @"^\d{9}$")
    | "cid" -> true
    | _ -> false

let allFieldsValid (passport:KVP list) =
    passport |> List.forall(isValidField)

kvpPassports
|> List.countBy (allFieldsValid)

printf "Testing..."
let testPassport = [{ key = "cid"; value = "181" };
                     { key = "iyr"; value = "2012" };
                     { key = "eyr"; value = "2024" };
                     { key = "byr"; value = "1934" };
                     { key = "hcl"; value = "#c0946f" };
                     { key = "hgt"; value = "165cm" };
                     { key = "ecl"; value = "oth" };
                     { key = "pid"; value = "232944581" }]
test <@ Regex.IsMatch("2012", @"\d{4}") = true @>
test <@ isYearBetween "2012" 2010 2020 @>
test <@ isYearBetween "2024" 2020 2030 @>
test <@ isYearBetween "1934" 1920 2002 @>
test <@ ((Regex.Match("165cm", @"(?<intHeight>\d+)(?<uint>in|cm)")).Success) = true @>
let aMatch = Regex.Match("165cm", @"(?<intHeight>\d+)(?<uint>in|cm)")
test <@ aMatch.Groups.["intHeight"].Value = "165" @>
test <@ int aMatch.Groups.["intHeight"].Value = 165 @>
test <@ aMatch.Groups.["uint"].Value = "cm" @>
test <@ isValidHeight "165cm" = true @>
test <@ Regex.IsMatch("#c0946f", @"#([0-9]|[a-f]){6}") = true @>
test <@ Regex.IsMatch("oth", @"(amb|blu|brn|gry|grn|hzl|oth)") = true @>
test <@ Regex.IsMatch("232944581", @"^\d{9}$") = true @>
test <@ Regex.IsMatch("0123456789", @"^\d{9}$") = false @>
test <@ allFieldsValid testPassport = true @>
test <@ isValidField {key="byr";value="2002"} = true @>
test <@ isValidField {key="byr";value="2003"} = false @>
test <@ isValidField {key="hgt";value="60in"} = true @>
test <@ isValidField {key="hgt";value="190in"} = false @>
test <@ isValidField {key="hgt";value="190cm"} = true @>
test <@ isValidField {key="hcl";value="#123abc"} = true @>
test <@ isValidField {key="hcl";value="#123abz"} = false @>
test <@ isValidField {key="hcl";value="123abc"} = false @>
test <@ isValidField {key="ecl";value="brn"} = true @>
test <@ isValidField {key="ecl";value="wat"} = false @>
test <@ isValidField {key="pid";value="000000001"} = true @>
test <@ isValidField {key="pid";value="0123456789"} = false @>
test <@ isValidField {key="pid";value="17324328"} = false @>
printfn "done!"
