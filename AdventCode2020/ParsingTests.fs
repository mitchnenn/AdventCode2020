namespace AdventCode2020

open FParsec
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit
open FParsecHelper

module ParsingTests =
    type ParsingTestsType(output:ITestOutputHelper) =
        
        [<Fact>]
        let ``FsUnit test`` () =
            1 |> should equal 1
            1 |> should not' (equal 2)
            test output pfloat "1.25" 
        
        let getMin : Parser<_> = pint32 .>> pchar '-'
        let getMax : Parser<_> = pint32 .>> spaces
        let getRange : Parser<_> = getMin .>>. getMax  

        [<Fact>]
        let ``Parse min max`` () =
            let input = "7-16 r: nvrrzhsgrwthgbrqkqh"
            test output getMin input
            test output (tuple2 getMin getMax) input
            test output getRange input
            Assert.True(true)
