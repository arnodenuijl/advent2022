module q15

open System
open System.IO
open System.Threading
open utils
open FParsec

let inputString = File.ReadAllText "15.txt"
// let inputString = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
// Sensor at x=9, y=16: closest beacon is at x=10, y=16
// Sensor at x=13, y=2: closest beacon is at x=15, y=3
// Sensor at x=12, y=14: closest beacon is at x=10, y=16
// Sensor at x=10, y=20: closest beacon is at x=10, y=16
// Sensor at x=14, y=17: closest beacon is at x=10, y=16
// Sensor at x=8, y=7: closest beacon is at x=2, y=10
// Sensor at x=2, y=0: closest beacon is at x=2, y=10
// Sensor at x=0, y=11: closest beacon is at x=2, y=10
// Sensor at x=20, y=14: closest beacon is at x=25, y=17
// Sensor at x=17, y=20: closest beacon is at x=21, y=22
// Sensor at x=16, y=7: closest beacon is at x=15, y=3
// Sensor at x=14, y=3: closest beacon is at x=15, y=3
// Sensor at x=20, y=1: closest beacon is at x=15, y=3"""

type Coord = {
    X : int64
    Y : int64
}
type SensorWithBeacon = {
    Sensor : Coord
    Beacon : Coord
}
type CoverageRange = {
    From : int64
    To : int64
} with
    member this.ItemsCount =
        1L + this.To - this.From     

type Coverage = {
    Y: int64
    Range: CoverageRange
}

let coordParser: Parser<Coord,unit> = skipString "x=" >>. pint32 .>> skipString ", y=" .>>. pint32 |>> fun (x,y) -> { X=x;Y=y }
let sensorsWithBeaconParser =
    skipString "Sensor at " >>. coordParser .>> skipString ": closest beacon is at " .>>. coordParser
    |>> fun (s, b) -> { Sensor = s;Beacon = b}

let mhDistance (sb : SensorWithBeacon) =
    abs (sb.Beacon.X - sb.Sensor.X) +
    abs (sb.Beacon.Y - sb.Sensor.Y) 

let rec coverageRanges (sb : SensorWithBeacon) ( y : int64)=
    let distance = mhDistance sb
    if  y < (sb.Sensor.Y - distance) || y > (sb.Sensor.Y + distance) then
        []
    else
        let width = distance - (abs (sb.Sensor.Y - y)) 
        let range = { From = sb.Sensor.X - width; To = sb.Sensor.X + width }
        [{Y = y; Range = range }]

let combineRanges (ranges : CoverageRange list) =
    let rec step (ranges : CoverageRange list) (stack : CoverageRange list) =
        match ranges, stack with
        | [], _ -> stack
        | toProcessHead :: toProcessTail, [] -> step toProcessTail  [toProcessHead] 
        | toProcessHead :: toProcessTail, stackHead :: stackTail ->
            if toProcessHead.From <= stackHead.To then // overlap
                let updatedTo = max stackHead.To toProcessHead.To
                let updatedStackHead = { From = stackHead.From ; To = updatedTo }
                step toProcessTail (updatedStackHead :: stackTail) 
            else
                step toProcessTail (toProcessHead :: stackHead :: stackTail) 
    let sortedRanges =
        ranges
         |> List.sortBy (fun x -> x.From)
    let merged = step sortedRanges []
    merged

let clipRanges (xMin : int64) (xMax: int64) (ranges : CoverageRange list)  =
    ranges
    |> List.collect (fun r ->
        if r.To < xMin || r.From > xMax then
            []
        else
            let newFrom = max r.From xMin
            let newTo = min r.To xMax
            [{From = newFrom ; To = newTo }]
            )
let takeOutPoints (xs : int64 list) (ranges : CoverageRange list) =     
    let takeOutSinglePoint (ranges : CoverageRange list) (x : int64)  =
        ranges
        |> List.collect (fun range ->
            if x < range.From || x > range.To then [range] // out of range
            else if x = range.From && x = range.To then [] // x equal to range of 1
            else if x = range.From then [{From = x + 1L; To = range.To}] // x is at the start
            else if x = range.To then [{From = range.From; To = x - 1L}] // x is at the end
            else [{From = range.From ; To = x - 1L } ; {From = x + 1L ; To = range.To}] // x is somewhere in the middle
            )        
    xs
    |> List.fold takeOutSinglePoint ranges
    
let sensorsWithBeaconsParser = sepBy sensorsWithBeaconParser skipNewline

let sensorsWithBeacons = run sensorsWithBeaconsParser inputString |> unwrapParserResult

let printRanges (ranges : CoverageRange list) =
    for r in ranges do Console.WriteLine $"{r.From} - {r.To}"
    

    
let q15a () =
    let positionsWithoutBeacon (sensorsWithBeacons : SensorWithBeacon list) (y : int) =
        let allRangesOnY: Coverage list =
            sensorsWithBeacons
            |> List.collect (fun sb -> coverageRanges sb y)

        let beaconsOnRow =
            sensorsWithBeacons
            |> List.filter (fun sb -> sb.Beacon.Y = y)
            |> List.map (fun sb -> sb.Beacon.X)
       
        let sensorsOnRow =
            sensorsWithBeacons
            |> List.filter (fun sb -> sb.Sensor.Y = y)
            |> List.map (fun sb -> sb.Sensor.X)
       
        let rangesOnRow =
            allRangesOnY
            |> List.map (fun cr -> cr.Range)
        
        combineRanges rangesOnRow
        |> takeOutPoints beaconsOnRow
        |> takeOutPoints  sensorsOnRow

    let result =
        positionsWithoutBeacon sensorsWithBeacons 2000000
        |> List.sumBy (fun x -> x.ItemsCount)
    Console.WriteLine $"15a: {result}"
    
let q15b () =
    let positionsWithoutBeaconBetween (sensorsWithBeacons : SensorWithBeacon list) (y : int64) (xMin: int64) (xMax : int64) =
        let allRangesOnY: Coverage list =
            sensorsWithBeacons
            |> List.collect (fun sb -> coverageRanges sb y)

        let rangesOnRow =
            allRangesOnY
            |> List.map (fun cr -> cr.Range)
        
        combineRanges rangesOnRow        
        |> clipRanges xMin xMax
    for y in [0 .. 4000000] do
        let ranges = positionsWithoutBeaconBetween sensorsWithBeacons y 0 4000000
        for r in ranges do
            if r <> { From = 0L ; To = 4000000L} then
                Console.WriteLine $"{y} {r.From} - {r.To}"
    Console.WriteLine "and then i wanted to go to bed and did the last step in excel :-)"