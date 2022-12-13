module q12

open System
open System.IO
open utils
open FSharp.Collections.ParallelSeq
open System.Threading
open System.Linq

let input = File.ReadAllLines "12.txt"
// let input = """Sabqponm
// abcryxxl
// accszExk
// acctuvwj
// abdefghi""" |> fun s ->  s.Split(Environment.NewLine)

let data = Array2D.create (input.[0].Length) input.Length ' '
type Coord = int * int
input 
|> Seq.iteri (fun y line -> 
    line 
    |> Seq.iteri (fun x c -> 
            data[x,y] <- c ))

let startPos = 
    seq {
        for x = 0 to (Array2D.length1 data) - 1 do
            for y = 0 to (Array2D.length2 data) - 1 do
                if data[x,y] = 'S' then
                    yield (x,y)
    } 
    |> Seq.exactlyOne

let endPos = 
    seq {
        for x = 0 to (Array2D.length1 data) - 1 do
            for y = 0 to (Array2D.length2 data) - 1 do
                if data[x,y] = 'E' then
                    yield (x,y)
    } 
    |> Seq.exactlyOne

data[fst startPos, snd startPos] <- 'a'
data[fst endPos, snd endPos] <- 'z'

let all = 
    seq {
        for x = 0 to (Array2D.length1 data) - 1 do
            for y = 0 to (Array2D.length2 data) - 1 do
                yield (x,y)
        
    }
    |> List.ofSeq

let allA = 
    all 
    |> List.filter (fun (x,y) -> data[x,y] = 'a')

let getPossibleNextSteps ((x,y: int) : Coord) : Coord list =
    let maxX = (Array2D.length1 data) - 1
    let maxY = (Array2D.length2 data) - 1
    [ (x - 1, y)
      (x + 1, y)
      (x, y - 1)
      (x, y + 1)]
    |> List.filter (fun (x,y) -> x >= 0 && y>= 0 && x <= maxX && y <= maxY)
    |> List.filter (fun (nextX,nextY) -> int data[nextX,nextY] <= int data[x,y] + 1)

let calculateDistances (startPos: Coord) (endPos: Coord) (skipNode : Char -> bool)=
    
    let rec processNode (fromPos: Coord) (unvisited: Set<Coord>) (distances : Map<Coord, int>) =
        // Console.WriteLine $"Process {fromPos} unvisited : {Set.count unvisited}"
        let nextNodes = 
            getPossibleNextSteps (fromPos : Coord)
            |> List.filter (fun c -> Set.contains c unvisited)   
        let updatedDistances =
            nextNodes
            |> List.fold (fun distances  c -> 
                            let currentDistanceNewNode = Map.find c distances
                            let distanceThrouhgCurrent = 1 + Map.find fromPos distances
                            // Console.WriteLine $"Checking {c}. currentDistanceNewNode: {currentDistanceNewNode}, distanceThrouhgCurrent: {distanceThrouhgCurrent} "
                            if distanceThrouhgCurrent < currentDistanceNewNode then
                                if c = endPos then Console.WriteLine $"Set {c} to {distanceThrouhgCurrent}"
                                // Console.WriteLine $"Set distance to {c} to {distanceThrouhgCurrent}"
                                Map.add c distanceThrouhgCurrent distances
                            else 
                                distances
                        
                         )
                         distances
        let updatedUnvisited = Set.remove fromPos unvisited
        if fromPos = endPos then 
            updatedDistances
        else
            let nextToProcess =
                updatedDistances
                |> Map.filter (fun coord dist -> Set.contains coord updatedUnvisited)
                |> Map.toSeq
                |> Seq.filter (fun (c, height) -> not <| skipNode (data[fst c, snd c]))
                |> Seq.sortBy snd
                |> Seq.tryHead
                
            match nextToProcess with 
            | Some (c,_) -> processNode c updatedUnvisited updatedDistances
            | None -> updatedDistances

    let unvisited = 
        all |> Set.ofSeq

    let distances = 
        all
        |> List.map (fun x -> x, 100000)
        |> Map.ofList
        |> Map.add startPos 0
    
    processNode startPos unvisited distances

let q12a() =
    // let endPos = (38,20)
    let allDistances = calculateDistances startPos endPos (fun _ -> false)
    let result = 
        Map.find endPos allDistances
    for d in allDistances do
        Console.WriteLine $"{d}"
    Console.WriteLine $"Start: {startPos}"
    Console.WriteLine $"End: {endPos}"
    Console.WriteLine $"12a: {result}"
    
let q12b() =
    let total = allA.Length
    Console.WriteLine $"{total} a's"
    let result = 
        allA
        |> PSeq.withExecutionMode ParallelExecutionMode.ForceParallelism
        |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered
        |> PSeq.withDegreeOfParallelism 20
        |> PSeq.mapi (fun i start -> 
            Console.WriteLine $"{i}: "
            calculateDistances start endPos (fun c -> c = 'a'))
        |> PSeq.map (fun x -> Map.find endPos x)    
        |> PSeq.filter (fun x -> x > 0)
        |> PSeq.min

    Console.WriteLine $"12b: {result}"
    ()
    