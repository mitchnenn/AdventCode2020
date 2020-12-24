#r @"/Users/mnenn/.nuget/packages/fsharp.text.regexprovider/2.1.0/lib/netstandard2.0/FSharp.Text.RegexProvider.dll"
open System.IO
open FSharp.Text.RegexProvider

let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = File.ReadAllLines(path)

type PolicyPassword = { pos1:int; pos2:int; character:char; password:string }

type PolicyPasswordRegex = Regex< @"^(?<Pos1>\d+)-(?<Pos2>\d+)\s(?<Character>[a-z]):\s(?<Password>.+)$" >
// Test
PolicyPasswordRegex().TypedMatch("7-12 h: hhhhhhdhhhhhfhhhh").Character.Value

let parsed =
    input
    |> Seq.map (PolicyPasswordRegex().TypedMatch)
    |> Seq.map (fun m -> {pos1=int m.Pos1.Value;pos2=int m.Pos2.Value;character=char m.Character.Value;password=string m.Password.Value} )
    |> Seq.toList

let isValidPassword (policyPassword:PolicyPassword) =
    let at1 = policyPassword.password.[policyPassword.pos1 - 1]
    let at2 = policyPassword.password.[policyPassword.pos2 - 1]
    (at1 = policyPassword.character && at2 <> policyPassword.character)
        || (at1 <> policyPassword.character && at2 = policyPassword.character)

parsed
|> List.filter (isValidPassword)
|> List.length

printf "Testing..."
printfn "done!"
