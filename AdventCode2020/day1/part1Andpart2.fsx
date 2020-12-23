#r @"/Users/mnenn/.nuget/packages/unquote/5.0.0/lib/netstandard2.0/Unquote.dll"
open Swensen.Unquote

let path = System.IO.Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = System.IO.File.ReadAllLines path |> Seq.map int

let rec getPairs list =
    let indexed = list |> List.indexed
    [ for(i,x) in indexed do
          for(_,y) in indexed.[i+1..] -> (x,y) ]

input
|> Seq.toList
|> getPairs
|> List.find (fun (x,y) -> x + y = 2020)
|> (fun (x,y) -> x * y) 

let rec getTriplets list =
    let indexed = list |> List.indexed
    [ for (i,x) in indexed do
          for (j,y) in indexed.[i+1..] do
              for (_,z) in indexed.[j+1..] -> (x,y,z)]

input
|> Seq.toList
|> getTriplets
|> List.find (fun (x,y,z) -> x + y + z = 2020)
|> (fun (x,y,z) -> x * y * z) 

printf "Testing..."
test <@ getPairs [1;2;3;4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)] @>
test <@ getTriplets [1;2;3;4] = [(1, 2, 3); (1, 2, 4); (1, 3, 4); (2, 3, 4)] @>
printfn "done!"
