module q17

open System
open System.IO
open System.Threading
open utils
open FParsec

// let jetPattern = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
let jetPattern = File.ReadAllText "17.txt"

let blocks = [
    [(0,0);(1,0);(2,0);(3,0)] |> Set.ofList
    [(1,0);(0,1);(1,1);(2,1);(1,2)] |> Set.ofList
    [(0,0);(1,0);(2,0);(2,1);(2,2)] |> Set.ofList
    [(0,0);(0,1);(0,2);(0,3)] |> Set.ofList
    [(0,0);(0,1);(1,0);(1,1)] |> Set.ofList
]

let ground = [(0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0)] |> Set.ofList


let top (w : Set<int * int>) =
    w
    |> Set.map snd // take y coord
    |> Set.maxElement
    
let bottom (w : Set<int * int>) =
    w
    |> Set.map snd // take y coord
    |> Set.minElement
    
let left (w : Set<int * int>) =
    w
    |> Set.map fst // take x coord
    |> Set.minElement
    
let right (w : Set<int * int>) =
    w
    |> Set.map fst // take x coord
    |> Set.maxElement

let moveX (delta : int) = Set.map (fun (x,y) -> ( x + delta, y))
let moveY (delta : int) = Set.map (fun (x,y) -> ( x, y + delta))
let moveBlock ((dx, dy) : int * int) (b : Set<int*int>) =
    Set.map( fun (x,y) -> (dx + x, dy + y)) b
let minX = 0
let maxX = 6

// try move down. Return the new block if possible, else None
let tryMoveDown (world : Set<int * int>) (b : Set<int * int>)  =
    let moved = (moveY -1 b)
    let isPossible = 
        moved
        |> Set.intersect world
        |> Set.isEmpty
    if isPossible then
        Some moved
    else None

// try move and return either the moved block or the original if the move is not possible
let tryMoveSideways (delta : int) (world: Set<int * int>) (b : Set<int * int>) =
    let moved = (moveX delta b)
    let doesntIntersectWithWorld = 
        moved
        |> Set.intersect world
        |> Set.isEmpty
    let doesntGoOffScreen = (right moved) <= maxX && (left moved) >= 0 
    if doesntIntersectWithWorld && doesntGoOffScreen then
        moved
    else b

    
let moveStream () =
    let patternLength = jetPattern.Length
    Seq.initInfinite (fun i ->
        let index = i % patternLength
        match jetPattern[index] with
        | '>' -> 1
        | '<' -> -1
        | _ -> failwith $"BAM!!! '{jetPattern[index]}'")

let cleanWorld (w : Set<int*int>) =
    let topOfTheWorld = top w
    w |> Set.filter (fun (_,y) -> y > (topOfTheWorld - 100))
let blockStream () =
    let blocksLength = blocks.Length
    Seq.initInfinite (fun i ->
        let index = i % blocksLength
        blocks[index])
    
let printWorld (world : Set<int*int>) =
    for y in [(top world) .. -1 .. (bottom world)] do
        Console.WriteLine ""
        for x in [minX..maxX] do
            if Set.contains (x,y) world then
                Console.Write "#"
            else 
                Console.Write "."
    Console.WriteLine ""

let runGame () =
    let moves = (moveStream ()).GetEnumerator()
    let nextMove () =
        moves.MoveNext() |> ignore
        moves.Current
        
    let blocks = (blockStream ()).GetEnumerator()
    let nextBlock () =
        blocks.MoveNext() |> ignore
        blocks.Current
    
    Seq.initInfinite id
    |> Seq.scan (fun (currentWorld : Set<int*int>) _ ->
            let block = nextBlock()
            let startPos = (2, (top currentWorld) + 4)
            let blockAtStart = moveBlock startPos block
            
            let rec step (b : Set<int*int>) (world : Set<int*int>) =
                let delta = nextMove()
                let movedSideways = tryMoveSideways delta world b
                match tryMoveDown world movedSideways with
                | Some newBlockAtPosition -> step newBlockAtPosition world
                | None -> movedSideways
                
            let placedBlock = step blockAtStart currentWorld
            Set.union placedBlock currentWorld
            |> cleanWorld

        )
        ground
    |> Seq.skip 1

let q17a () =
    let lastWorld =
        runGame ()
        |> Seq.take 2022
        |> Seq.last
    let result = (top lastWorld)
    Console.WriteLine $"17a: {result}"
    
let q17b () =
    
    // let mutable lastHeight = 0;
    // let heigts =
    //     runGame ()
    //     |> Seq.take 1_000_000
    //     |> Seq.map top
    //     |> Seq.toArray
    //
    // let reverseHeights = Array.rev heigts
    // [5..10000]
    // |> List.iter(fun chunck ->
    //     if reverseHeights[0] - reverseHeights[chunck] = reverseHeights[chunck] - reverseHeights[2 * chunck] &&
    //        reverseHeights[chunck] - reverseHeights[2 * chunck] = reverseHeights[2 * chunck] - reverseHeights[3 * chunck] &&
    //        reverseHeights[2 * chunck] - reverseHeights[3 * chunck] = reverseHeights[3 * chunck] - reverseHeights[4 * chunck]  then 
    //         Console.WriteLine chunck
    //     )
    
    // every 1720 it repeats
//     1720
// 3440
// 5160
// 6880
// 8600
    
    // let offset =
    //     1000000000000L % 1720L 
    //     |> int32
    // Console.WriteLine offset // 1440
    //
    // let mutable lastHeight = 0;
    //
    // runGame()
    // |> Seq.iteri(fun i world ->
    //     if (i - 1440) % 1720 = 0 then
    //         let height = top world
    //         Console.WriteLine $"{i}: {height} (delta: {height - lastHeight})"
    //         lastHeight <- height
    //     )
    //
    // delta = 2704
    
    let timesDelta = (1000000000000L - 1440L) / 1720L
    Console.WriteLine $"timesDelta = {timesDelta}"
    
    let result = ((timesDelta * 2704L) + 2276L) - 1L // no idea why the -1 :-)
    Console.WriteLine $"17b: {result}"

    ()