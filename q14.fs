module q14

open System
open System.IO
open System.Threading
open utils
open FParsec

let inputString = File.ReadAllText "14.txt"

type Point = int * int
type Structure = Point list
let coordParser: Parser<Point, unit> = pint32 .>> skipString "," .>>. pint32
let structureParser: Parser<Point list,unit> = sepBy coordParser (pstring " -> ")
let structuresParserParser: Parser<Structure list,unit> = sepBy structureParser skipNewline
let input = run structuresParserParser inputString |> unwrapParserResult

let getStructureCoords (s: Structure) =
    s
    |> List.windowed 2
    |> List.collect (fun [ (x1, y1); (x2, y2) ] ->
        seq {
            for x in [ (min x1 x2) .. (max x1 x2) ] do
                for y in [ (min y1 y2) .. (max y1 y2) ] do
                    yield (x, y)
        }
        |> Seq.toList)
    |> Set.ofList

let sandStart = (500,0)

let allRockPoints: Set<int * int> =
    input
    |> List.map getStructureCoords
    |> Set.unionMany

let q14a () =
    let dropSand (rockPoints : Set<Point>) =
        let maxY = 1 + (rockPoints.Add sandStart |> Set.toList |> List.map snd |> List.max)

        let isFree p sandPoints  =
            not <| Set.contains p rockPoints &&
            not <| Set.contains p sandPoints
            
        let rec sandFallStep (stationarySandPoints : Set<Point>) (sx, sy) =
            if sy > (maxY + 1) then None
            else if isFree (sx, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx, sy + 1) 
            else if isFree (sx - 1, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx - 1, sy + 1)
            else if isFree (sx + 1, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx + 1, sy + 1)
            else Some (sx,sy)

        let rec releaseSandUnit sandPoints =
            match sandFallStep sandPoints sandStart with
            | None -> sandPoints
            | Some sandDropSpot -> releaseSandUnit (Set.add sandDropSpot sandPoints) 
        releaseSandUnit Set.empty
    
    let result = dropSand allRockPoints
    Console.WriteLine $"{Set.count result}"

let q14b () =
    let dropSand (rockPoints : Set<Point>) =
        let maxY = (rockPoints.Add sandStart |> Set.toList |> List.map snd |> List.max)

        let isFree p sandPoints  =
            not <| Set.contains p rockPoints &&
            not <| Set.contains p sandPoints
            
        let rec sandFallStep (stationarySandPoints : Set<Point>) (sx, sy) =
            if sy = (maxY + 1) then (sx,sy)
            else if isFree (sx, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx, sy + 1) 
            else if isFree (sx - 1, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx - 1, sy + 1)
            else if isFree (sx + 1, sy + 1) stationarySandPoints then sandFallStep stationarySandPoints (sx + 1, sy + 1)
            else (sx,sy)

        let rec releaseSandUnit sandPoints =
            match sandFallStep sandPoints sandStart with
            | 500,0 -> Set.add sandStart sandPoints
            | otherSandDropSpot ->
                releaseSandUnit (Set.add otherSandDropSpot sandPoints) 
            
        releaseSandUnit Set.empty
    
    let allRockPoints: Set<int * int> =
        input
        |> List.map getStructureCoords
        |> Set.unionMany
    let result = dropSand allRockPoints
    Console.WriteLine $"{Set.count result}"
