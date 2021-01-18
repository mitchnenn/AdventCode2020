#r @"/Users/mnenn/.nuget/packages/unquote/5.0.0/lib/netstandard2.0/Unquote.dll"

open System.IO 
open Swensen.Unquote

let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = File.ReadAllLines(path)


let groups = input |> Seq.groupBy(fun i -> "")


printf "Testing..."
test <@ 1 + 1 = 2 @>
printfn "done!"
