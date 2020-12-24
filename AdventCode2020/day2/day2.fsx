#r @"/Users/mnenn/.nuget/packages/fsharp.text.regexprovider/2.1.0/lib/netstandard2.0/FSharp.Text.RegexProvider.dll"
open System.IO
open FSharp.Text.RegexProvider

let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = File.ReadAllLines(path)

type PolicyPassword = { min:int; max:int; character:char; password:string }

type PolicyPasswordRegex = Regex< @"^(?<Min>\d+)-(?<Max>\d+)\s(?<Character>[a-z]):\s(?<Password>.+)$" >
// Test
PolicyPasswordRegex().TypedMatch("7-12 h: hhhhhhdhhhhhfhhhh").Character.Value

let parsed =
    input
    |> Seq.map (PolicyPasswordRegex().TypedMatch)
    |> Seq.map (fun m -> {min=int m.Min.Value;max=int m.Max.Value;character=char m.Character.Value;password=string m.Password.Value} )
    |> Seq.toList

let isValidPassword (policyPassword:PolicyPassword) =
    let occurrences = policyPassword.password
                     |> Seq.filter (fun c -> c = policyPassword.character)
                     |> Seq.length
    occurrences >= policyPassword.min && occurrences <= policyPassword.max

parsed
|> List.filter (isValidPassword)
|> List.length

printf "Testing..."
printfn "done!"
