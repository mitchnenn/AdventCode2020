namespace AdventCode2020

open System.IO
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module BoardingPassTests =
    type BoardingPassTestsType(output:ITestOutputHelper) =
        let rowInputCount = 7
        let seatInputCount = 3
        let numRows = 128
        let numSeatsPerRow = 8

        let getRowCodes (boardingPass:string) =
            boardingPass.ToCharArray()
            |> Seq.take(rowInputCount)
            |> Seq.toList
            
        let getSeatCodes (boardingPass:string) =
            boardingPass.ToCharArray()
            |> Seq.rev
            |> Seq.take(seatInputCount)
            |> Seq.rev
            |> Seq.toList
                
        [<Fact>]
        let ``Parse boarding pass input test`` () =
            let boardingPass = "FBFBBFFRLR"
            getRowCodes boardingPass |> should equal ['F';'B';'F';'B';'B';'F';'F']
            getSeatCodes boardingPass |> should equal ['R';'L';'R']
            
        let calcDiff (startIndex:int) (endIndex:int) =
            (endIndex + 1 - startIndex) / 2
        
        let calcLowerIndex (startIndex:int) (endIndex:int) =
            let diff = calcDiff startIndex endIndex
            let newEndIndex = endIndex - diff
            (startIndex, newEndIndex)

        let calcUpperIndex (startIndex:int) (endIndex:int) =
            let diff = calcDiff startIndex endIndex
            let newStartIndex = startIndex + diff
            (newStartIndex, endIndex)
        
        let calculateNewIndexes (direction:char) startIndex endIndex =
             match direction with
             | 'F' | 'L' -> calcLowerIndex startIndex endIndex
             | 'B' | 'R' -> calcUpperIndex startIndex endIndex
             | _ -> failwith "Bad input"
        
        let getRowOrSeatIndex (codes:char list) startIndex endIndex =
            let rec loop i startI endI =
                match i with
                | [] -> startI
                | x :: xs ->
                     let (newStartIndex, newEndIndex) = calculateNewIndexes x startI endI  
                     loop xs newStartIndex newEndIndex 
            loop codes startIndex endIndex
            
        [<Theory>]
        [<InlineData("FBFBBFFRLR", 44)>]
        [<InlineData("BFFFBBFRRR", 70)>]
        [<InlineData("FFFBBBFRRR", 14)>]
        [<InlineData("BBFFBBFRLL", 102)>]
        let ``Parse boarding pass row location`` (boardingPass:string, rowIndex:int) =
            let codes = getRowCodes boardingPass
            let actualRow = getRowOrSeatIndex codes 0 (numRows - 1)
            actualRow |> should equal rowIndex
            
        [<Theory>]
        [<InlineData("FBFBBFFRLR", 5)>]
        [<InlineData("BFFFBBFRRR", 7)>]
        [<InlineData("FFFBBBFRRR", 7)>]
        [<InlineData("BBFFBBFRLL", 4)>]
        let ``Parse boarding pass seat location`` (boardingPass:string, seatIndex:int) =
            let codes = getSeatCodes boardingPass
            let actualSeat = getRowOrSeatIndex codes 0 (numSeatsPerRow - 1)
            actualSeat |> should equal seatIndex

        let parseBoardingPassLocation (boardingPass:string) =
            let rowCodes = getRowCodes boardingPass
            let rowIndex = getRowOrSeatIndex rowCodes 0 (numRows - 1)
            let seatCodes = getSeatCodes boardingPass
            let seatIndex = getRowOrSeatIndex seatCodes 0 (numSeatsPerRow - 1)
            let uniqueId = (rowIndex * 8) + seatIndex
            (rowIndex, seatIndex, uniqueId)
        
        [<Theory>]
        [<InlineData("FBFBBFFRLR", 44, 5, 357)>]
        [<InlineData("BFFFBBFRRR", 70, 7, 567)>]
        [<InlineData("FFFBBBFRRR", 14, 7, 119)>]
        [<InlineData("BBFFBBFRLL", 102, 4, 820)>]
        let ``Parse seat location test`` (boardingPass:string, rowIndex:int, seatIndex:int, id:int) =
            let (actualRowIndex, actualSeatIndex, actualId) = parseBoardingPassLocation boardingPass
            actualRowIndex |> should equal rowIndex
            actualSeatIndex |> should equal seatIndex
            actualId |> should equal id
            
        let path = Path.Combine($@"{__SOURCE_DIRECTORY__}", "day5", "input.txt")
        
        let third t =
            let (_,_,s) = t
            s
        
        let getAllBoardingPasses = 
            File.ReadAllLines(path)
            |> Seq.map(parseBoardingPassLocation)
            |> Seq.sortBy(third)
            |> Seq.toList
        
        [<Fact>]
        let ``Get highest seat index`` () =
            let (_,_,highestSeatId) = getAllBoardingPasses
                                      |> List.rev
                                      |> List.head
            highestSeatId |> should equal 911
            
        [<Fact>]
        let ``Missing seat test`` () =
            let seatIds = getAllBoardingPasses
                          |> List.map(third)
            let missingSeatIds = [(seatIds |> List.head)..(seatIds |> List.last)]
                                 |> List.except seatIds
            missingSeatIds |> List.length |> should equal 1
            missingSeatIds |> List.head |> should equal 629
            