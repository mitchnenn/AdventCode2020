#r @"nuget: Unquote"
open Swensen.Unquote

let path = System.IO.Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = System.IO.File.ReadAllLines path |> Seq.map int

let rec combinations list =
    match list with
    | [] -> []
    | x :: xs ->
            let firstParis = xs |> List.map (fun y -> (x,y))
            firstParis @ combinations xs

input
|> Seq.toList
|> combinations
|> List.find (fun (x,y) -> x + y = 2020)
|> (fun (x,y) -> x * y) 

let rec combinations3 list =
    match list with
    | [] -> []
    | x :: xs ->
            let combos = combinations xs
            let firstParis = combos |> List.map (fun (y,z) -> (x,y,z))
            firstParis @ combinations3 xs

input
|> Seq.toList
|> combinations3
|> List.find (fun (x,y,z) -> x + y + z = 2020)
|> (fun (x,y,z) -> x * y * z) 

printf "Testing..."
test <@ combinations [1;2;3;4] = [(1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4)] @>
test <@ combinations3 [1;2;3;4] = [(1, 2, 3); (1, 2, 4); (1, 3, 4); (2, 3, 4)] @>
printfn "done!"
