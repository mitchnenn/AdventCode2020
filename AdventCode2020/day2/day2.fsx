#r @"/Users/mnenn/.nuget/packages/unquote/5.0.0/lib/netstandard2.0/Unquote.dll"
#r @"/Users/mnenn/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsec.dll"
#r @"/Users/mnenn/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0/FParsecCS.dll"

open Swensen.Unquote
open FParsec

let first (c, _, _) = c
let second (_, c, _) = c
let third (_, _, c) = c

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let testp p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

testp pfloat "1.25"

let path = System.IO.Path.Combine($@"{__SOURCE_DIRECTORY__}", "input.txt")

type Policy = {Min:int; Max:int; C:char}
type Password = {thePolicy:Policy; Password:string}

let getMin : Parser<_> = pint32 .>> pchar '-'
let getMax : Parser<_> = pint32 .>> spaces
let getRange : Parser<_> = getMin .>>. getMax  
let getChar : Parser<_> = anyChar .>> skipChar ':'

let getPolicy (input:string) =
    let result = run (tuple3 getMin getMax getChar) input
    match result with
    | Success(p,_,_) -> {Min=(first p);Max=(second p);C=(third p)}
    | Failure(msg,_,_) -> failwith msg

printf "Testing..."
testp getMin "7-16 r: nvrrzhsgrwthgbrqkqh"
testp (tuple2 getMin getMax) "7-16 r: nvrrzhsgrwthgbrqkqh"
testp getRange "7-16 r: nvrrzhsgrwthgbrqkqh"
testp (tuple3 getMin getMax getChar) "7-16 r: nvrrzhsgrwthgbrqkqh"
getPolicy "7-16 r: nvrrzhsgrwthgbrqkqh"
printfn "done!"