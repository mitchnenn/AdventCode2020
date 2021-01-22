namespace AdventCode2020

open System
open System.IO
open Xunit
open Xunit.Abstractions
open FSharp.Text.RegexProvider
open FsUnit.Xunit

type Instruction = {op:string; arg:int}
type InstructionRegex = Regex< @"^(?<Op>.*)\s(?<Arg>.*)$" >
type ProgramInstruction = {lineNum:int; instr:Instruction}
type Program = {lines:ProgramInstruction list}

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
                    executed |> List.rev, -1
                | index when index = instrCount ->
                    executed |> List.rev, acc 
                | index ->
                    let currentInstruction = instructions.[index]
                    match getNextIndex currentInstruction index with
                    | next when executed |> List.contains next ->
                        executed |> List.rev, -1
                    | next -> loop next (index::executed) (calcAcc currentInstruction acc)
            loop 0 [] 0
        
        let createProgramListing (instructions:Instruction list) =
            seq {
                for l in [1..(instructions |> List.length)] do
                    yield {lineNum=l; instr=instructions.[l-1]}
            } |> Seq.toList
        
        let changeJmpInstruction (original:ProgramInstruction list) (jmpInstr:ProgramInstruction) =
            let nop = {op="nop"; arg=jmpInstr.instr.arg}
            let nopInstr = {lineNum=jmpInstr.lineNum; instr=nop}
            original
            |> List.map (fun line -> if line.lineNum = jmpInstr.lineNum then nopInstr else line)
        
        let createProgramJmpVariants (original:Program) : Program list =
            let allJumpLines = original.lines
                               |> Seq.where (fun l -> l.instr.op = "jmp")
                               |> Seq.toList
            allJumpLines
            |> List.map (changeJmpInstruction original.lines)
            |> List.map (fun li -> {lines=li})
        
        let executeAProgram (program:Program) =
            let instructions = program.lines |> List.map(fun line -> line.instr)
            let result = execute instructions
            snd result
        
        [<Fact>]
        let ``Execute example test`` () =
            let instructions = example.Split(Environment.NewLine)
                               |> Seq.map parseInstruction
                               |> List.ofSeq
            let program = {lines=createProgramListing instructions}
            let variantPrograms = createProgramJmpVariants program
            let result = variantPrograms |> List.map (executeAProgram)
            let answer = result |> Seq.where (fun i -> i <> -1) |> Seq.head
            output.WriteLine(sprintf "%i" answer)
            answer |> should equal 8
        
        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day8", "input.txt")
        let input = File.ReadLines(path)

        [<Fact>]
        let ``Execute input test`` () =
            let instructions = input
                               |> Seq.map parseInstruction
                               |> List.ofSeq
            let program = {lines=createProgramListing instructions}
            let variantPrograms = createProgramJmpVariants program
            let result = variantPrograms |> List.map (executeAProgram)
            let answer = result |> Seq.where (fun i -> i <> -1) |> Seq.head
            output.WriteLine(sprintf "%i" answer)
            answer |> should equal 2060
