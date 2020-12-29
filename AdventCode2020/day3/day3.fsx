#r @"/Users/mnenn/.nuget/packages/unquote/5.0.0/lib/netstandard2.0/Unquote.dll"
open Swensen.Unquote
open System.IO

let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")
let input = File.ReadAllLines(path)

type Space = Tree | Empty
type Forest = Space list list

let parseForest (lines:string seq) =
    [
        for row in lines ->
        [    for sp in row ->
                match sp with
                | '.' -> Empty
                | '#' -> Tree
                | _ -> failwith "unknown input"
        ]
    ]

let forest =
    input
    |> parseForest

let elementAt (forest:Forest) (x,y) =
    if y >= forest.Length then
        None
    else
        let row = forest.[y]
        Some row.[x % row.Length]

type State = {location:(int*int);visited:Space list}

let moveSpace (forest:Forest) (m:int*int) (state:State) =
    let (x,y) = state.location
    let newLocation = (x+fst m,y+snd m)
    let element = elementAt forest newLocation
    element
    |> Option.map(fun el -> 
        let next = { state with location = newLocation; visited = el::state.visited }
        next, next)

let getVisitedWithTrees (m:int*int) =
    let endState = Seq.unfold (moveSpace forest m) {location=(0,0);visited=[]}
                   |> Seq.last
    endState.visited
    |> List.filter(function | Tree -> true | _ -> false)
    |> List.length

[(1,1);(3,1);(5,1);(7,1);(1,2)]
    |> Seq.map(getVisitedWithTrees)
    |> Seq.map(float)
    |> Seq.reduce (*)

printf "Testing..."
let testGrid = [".#";"##"]
test <@ parseForest testGrid = [[Empty; Tree]; [Tree; Tree]]  @>
let testForest = parseForest testGrid
test <@ elementAt testForest (0,0) = Some Empty @>
test <@ elementAt testForest (1,1) = Some Tree @>
test <@ elementAt testForest (2,0) = Some Empty @>
test <@ elementAt testForest (3,0) = Some Tree @>
test <@ elementAt testForest (3,2) = None @>
test <@ getVisitedWithTrees (3,1) = 211 @>
printfn "done!"
