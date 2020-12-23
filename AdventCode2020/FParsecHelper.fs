namespace AdventCode2020

open FParsec
open Xunit.Abstractions

module FParsecHelper = 

    type UserState = unit // doesn't have to be unit, of course
    type Parser<'t> = Parser<'t, UserState>
        
    let test (output:ITestOutputHelper) p str =
        match run p str with
        | Success(result, _, _)   -> output.WriteLine(sprintf "Success: %A" result)
        | Failure(errorMsg, _, _) -> output.WriteLine(sprintf "Failure: %s" errorMsg)
