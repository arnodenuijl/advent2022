module q9

open System
open System.IO
open utils
open FParsec

let input = File.ReadAllText "09.txt"

type Move = 
| Up of int
| Down of int
| Left of int
| Right of int

let moveParser = choice [
        pstring "U " >>. pint32 |>> Up
        pstring "D " >>. pint32 |>> Down
        pstring "L " >>. pint32 |>> Left
        pstring "R " >>. pint32 |>> Right ] 

let moveHead (move: Move) (fromX : int, fromY : int) =
    match move with 
    | Up steps -> [1..steps] |> Seq.map (fun s -> (fromX, fromY - s))
    | Down steps -> [1..steps] |> Seq.map (fun s -> (fromX, fromY + s))
    | Left steps -> [1..steps] |> Seq.map (fun s -> (fromX - s, fromY))
    | Right steps -> [1..steps] |> Seq.map (fun s -> (fromX + s, fromY))
    |> List.ofSeq

let moveHeadMany (moves: Move list) (start : int * int) =
    moves 
    |> List.fold
        (fun (allPositions : (int*int) list) (move : Move) -> 
            let lastIndex = allPositions.Length - 1
            let current = allPositions[lastIndex]
            allPositions @ moveHead move current) 
        [start]

let inline distance (xa, ya) (xb, yb) = 
    pown (float xa - float xb) 2 + pown (float ya - float yb) 2
    |> sqrt

let moveTail 
        (currentHeadX : int, currentHeadY : int) 
        (currentTailX : int, currentTailY : int) =
    let deltaX = currentHeadX - currentTailX
    let deltaY = currentHeadY - currentTailY
    
    if abs deltaX > 1 || abs deltaY > 1 then
        seq {
            for dx in [-1..1] do
            for dy in [-1..1] do
                yield (currentTailX + dx, currentTailY + dy)
        } 
        |> Seq.sortBy (fun pos -> distance pos (currentHeadX, currentHeadY))
        |> Seq.head
    else 
        (currentTailX, currentTailY)

let trailingPath (path : (int * int) list) = 
        path
        |> List.windowed 2
        |> List.scan 
            (fun currentTail [previousHead;currentHead] -> 
                moveTail currentHead currentTail)
            path.Head
    
let moves = run (sepBy moveParser newline) input |> unwrapParserResult

let headPath = moveHeadMany moves (0,0)    

let q9a() =
    let uniquePositions = trailingPath headPath |> Set.ofSeq |> Seq.length
    Console.WriteLine $"9a: {uniquePositions}"

let q9b() =
    let uniquePositions = 
        [1..9]
        |> Seq.fold (fun previousPath _ -> 
                    trailingPath previousPath)
                    headPath
        |> Set.ofSeq |> Seq.length
    Console.WriteLine $"9b: {uniquePositions}"
