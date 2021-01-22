namespace AdventCode2020

open System
open System.IO
open Xunit
open Xunit.Abstractions
open FSharp.Text.RegexProvider
open FsUnit.Xunit

type Instruction = {op:string; arg:int}
type InstructionRegex = Regex< @"^(?<Op>.*)\s(?<Arg>.*)$" >

module GameTests =
    type GamesTestsType(output:ITestOutputHelper) =

        let example = "nop +0" + Environment.NewLine
                    + "acc +1" + Environment.NewLine
                    + "jmp +4" + Environment.NewLine
                    + "acc +3" + Environment.NewLine
                    + "jmp -3" + Environment.NewLine
                    + "acc -99" + Environment.NewLine
                    + "acc +1" + Environment.NewLine
                    + "jmp -4" + Environment.NewLine
                    + "acc +6"

        let parseInstruction instr : Instruction =
            InstructionRegex().TypedMatch(instr) 
            |> (fun m -> {op=m.Op.Value; arg=(int)m.Arg.Value})
        
        let calcAcc (instr:Instruction) currentAcc =
            if instr.op = "acc" then
                currentAcc + (int)instr.arg
            else
                currentAcc
        
        let getNextIndex (currentInstruction:Instruction) currentIndex =
            if currentInstruction.op = "jmp" then
                currentIndex + (int)currentInstruction.arg
            else
                currentIndex + 1
        
        let execute (instructions:Instruction list) =
            let instrCount = instructions |> List.length
            let rec loop instrIndex executed acc =
                match instrIndex with
                | index when index < 0 || index > instrCount ->
                    failwith (sprintf "index out of range: %i" index)
                | index when index = instrCount ->
                    output.WriteLine("done")
                    executed |> List.rev, acc 
                | index ->
                    let currentInstruction = instructions.[index]
                    match getNextIndex currentInstruction index with
                    | next when executed |> List.contains next ->
                        output.WriteLine("Infinite loop detected")
                        executed |> List.rev, acc
                    | next -> loop next (index::executed) (calcAcc currentInstruction acc)
            loop 0 [] 0
        
        [<Fact>]
        let ``Execute example test`` () =
            let instructions = example.Split(Environment.NewLine)
                               |> Seq.map parseInstruction
                               |> List.ofSeq
            let program = execute instructions
            output.WriteLine(sprintf "%A" (fst program))
            output.WriteLine(sprintf "%i" (snd program))
            snd program |> should equal 5
        
        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day8", "input.txt")
        let input = File.ReadLines(path)

        [<Fact>]
        let ``Execute input test`` () =
            let instructions = input
                               |> Seq.map parseInstruction
                               |> List.ofSeq
            let program = execute instructions
            output.WriteLine(sprintf "%A" (fst program))
            output.WriteLine(sprintf "%i" (snd program))
            snd program |> should equal 1801
