module q5

open System
open System.IO

let createStacks (startPositionLines: char array array) =
    let getStackAtPos i =
        startPositionLines
        |> Array.map (fun line -> line[i])
        |> Array.filter (fun c -> c <> ' ')
        |> List.ofArray 

    [|
        getStackAtPos 1
        getStackAtPos 5
        getStackAtPos 9
        getStackAtPos 13
        getStackAtPos 17
        getStackAtPos 21
        getStackAtPos 25
        getStackAtPos 29
        getStackAtPos 33
    |]    

type Move = {
    MoveCount : int
    FromStack : int
    ToStack : int 
}

let createMoves moveLines = 
    let getMoveFromLine (s: string) = 
        let numbers = s.Replace("move ", "").Replace("from ", "").Replace("to ","").Split(" ") 
        { MoveCount = int (numbers[0])
          FromStack = (int (numbers[1])) - 1
          ToStack = (int (numbers[2])) - 1 }

    moveLines
    |> Array.map getMoveFromLine

let doMove (move: Move) (stacks: List<Char> array) =
    let blocksRemoved = stacks[move.FromStack][..move.MoveCount - 1] |> List.rev
    let stackWithBlocksRemoved = stacks[move.FromStack][move.MoveCount ..]
    let stackWithBlockAdded = blocksRemoved @ stacks[move.ToStack]
    stacks[move.FromStack] <- stackWithBlocksRemoved
    stacks[move.ToStack] <- stackWithBlockAdded

let doMoveAllAtOnce (move: Move) (stacks: List<Char> array) =
    let blocksRemoved = stacks[move.FromStack][..move.MoveCount - 1] 
    let stackWithBlocksRemoved = stacks[move.FromStack][move.MoveCount ..]
    let stackWithBlockAdded = blocksRemoved @ stacks[move.ToStack]
    stacks[move.FromStack] <- stackWithBlocksRemoved
    stacks[move.ToStack] <- stackWithBlockAdded

let printStacks stacks = 
    for stack in stacks do  
        for c in stack do Console.Write $" {c}"
        Console.WriteLine ""
    Console.WriteLine ""

let allLines: string[] = File.ReadAllLines "05.txt"
let moveLines = allLines[10..] 
let startPositionLines =
    allLines[..7] 
    |> Array.map (fun s -> s.ToCharArray())

let q5a() = 
    let stacks: List<Char> array = createStacks startPositionLines
    let moves: Move[] = createMoves moveLines
    for m in moves do
        doMove m stacks
    printStacks stacks
   
let q5b() = 
    let stacks: List<Char> array = createStacks startPositionLines
    let moves: Move[] = createMoves moveLines

    for m in moves do
        doMoveAllAtOnce m stacks
    printStacks stacks 
