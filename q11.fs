module q11

open System
open System.IO
open utils
open FParsec

let input = File.ReadAllText "11.txt"

type Operation = 
| Add of uint64
| Multiply of uint64
| Pow2


type Monkey = {
    Items : uint64 list
    Operation : Operation
    DivisorTest : uint64
    MonkeyThrowTrue : int
    MonkeyThrowFalse : int
}

type MonkeyAction = { 
    ToMonkey : int
    Item : uint64}

module Parsing =
    let monkeyNumberParser = pstring "Monkey " >>. pint32 .>> pstring ":" 
    let startingItemsParser = spaces >>. pstring "Starting items: " >>. sepBy puint64 (pstring ", ") |>> List.map uint64
    let operationParser = 
        spaces >>. choice [
            spaces >>. pstring "Operation: new = old * old" |>> fun _ -> Pow2
            spaces >>. pstring "Operation: new = old + " >>. puint64 |>> (fun x ->  Add (uint64 x))
            spaces >>. pstring "Operation: new = old * " >>. puint64 |>> (fun x -> Multiply (uint64 x))
        ]
    let divisorParser = spaces >>. pstring "Test: divisible by " >>. puint64
    let monkeyTrueParser = spaces >>. pstring "If true: throw to monkey " >>. pint32
    let monkeyFalseParser = spaces >>. pstring "If false: throw to monkey " >>. pint32
    let monkeyParser = 
        monkeyNumberParser 
            .>> skipNewline .>>. startingItemsParser 
            .>> skipNewline .>>. operationParser 
            .>> skipNewline .>>. divisorParser 
            .>> skipNewline .>>. monkeyTrueParser 
            .>> skipNewline .>>. monkeyFalseParser
            |>> fun (((((monkeyNumber, startingItems), operation), divisor), mTrue), mFalse) -> 
                (monkeyNumber, {
                                Monkey.Items = startingItems
                                Monkey.Operation = operation
                                Monkey.DivisorTest = divisor
                                Monkey.MonkeyThrowTrue = mTrue
                                Monkey.MonkeyThrowFalse = mFalse
                                }) 
    let monkeysParser = sepBy monkeyParser (skipNewline >>. skipNewline)                      

let doOperation (operation: Operation) (inputValue : uint64) =
    match operation with 
    | Pow2 -> inputValue * inputValue
    | Add x -> inputValue + x
    | Multiply x -> inputValue * x 

let createMonkeyActions (manageWorryLevel : uint64 -> uint64) (monkey: Monkey) = 
    let actions = 
        monkey.Items
        |> List.map (fun item -> 

                        let newWorry = 
                            (doOperation monkey.Operation item)
                            |> manageWorryLevel

                        if newWorry % monkey.DivisorTest  = (uint64 0) then  
                            { MonkeyAction.ToMonkey = monkey.MonkeyThrowTrue
                              MonkeyAction.Item = newWorry}
                        else 
                            { MonkeyAction.ToMonkey = monkey.MonkeyThrowFalse
                              MonkeyAction.Item = newWorry}
                        )
    actions

let processMonkey (manageWorryLevel : uint64 -> uint64) (monkeyNuber:int) (monkeys : Map<int, Monkey>) (itemsIspected : Map<int, uint64>)=
    let updatedIemsInspected = 
        itemsIspected
        |> Map.change monkeyNuber (fun inspectedOption -> 
                                            let currentCount = Option.defaultValue (uint64 0) inspectedOption
                                            currentCount + uint64 monkeys[monkeyNuber].Items.Length
                                            |> Some)

    let updatedMonkeys = 
        let actions = createMonkeyActions manageWorryLevel monkeys[monkeyNuber]
        actions
        |> List.fold
            (fun (ms:Map<int, Monkey>) (action: MonkeyAction) -> 
                let monkeyToThrowTo = ms[action.ToMonkey]
                ms 
                |> Map.add action.ToMonkey { monkeyToThrowTo with Items = monkeyToThrowTo.Items @ [action.Item] }
            )
            monkeys
        
        |> Map.add monkeyNuber ({monkeys[monkeyNuber] with Items = []}) 
    (updatedMonkeys, updatedIemsInspected)

let doRun (manageWorryLevel : uint64 -> uint64) (monkeys : Map<int, Monkey>) (monkeysInspected: Map<int,uint64>)= 
    [0..(monkeys.Count - 1)]
    |> List.fold 
                (fun (monkeys, itemsInspected) monkeyNumberToProcess -> 
                        processMonkey manageWorryLevel monkeyNumberToProcess monkeys itemsInspected)
                (monkeys, monkeysInspected)

let printMonkeys (monkeys : Map<int, Monkey>) = 
    [0..(monkeys.Count - 1)]
    |> List.iter (fun i -> 
        let m = monkeys[i]
        let items = String.Join(", " , m.Items)
        Console.WriteLine $"{i}: {items}"
        )
let printItemsInspected (itemsInspected: Map<int,uint64>) = 
    [0..(itemsInspected.Count - 1)]
    |> List.iter (fun i -> 
        Console.WriteLine $"{i} {itemsInspected[i]}"
    )

let q11a() = 
    let monkeys = run Parsing.monkeysParser input |> unwrapParserResult |> Map.ofList
        
    let manageWorryLevel = fun (x:uint64) -> x / (uint64 3)

    let result = 
        [1..20] 
        |> List.fold 
            (fun (monkeys, itemsInspected) i  -> doRun manageWorryLevel monkeys itemsInspected)
            (monkeys, Map.empty)
    
    let answer = 
        (snd result)
        |> Map.toList
        |> List.map snd
        |> List.sortDescending
        |> List.take 2
        |> List.reduce (*)
    
    Console.WriteLine $"11a: {answer}"
    
let q11b() = 
    let monkeys = 
        run Parsing.monkeysParser input |> unwrapParserResult |> Map.ofList
    
    let divisionProduct = 
        monkeys
        |> Map.toList
        |> List.map snd
        |> List.map (fun m -> m.DivisorTest)
        |> List.reduce (*)

    let manageWorryLevel = fun (x:uint64) -> x % divisionProduct

    let result = 
        [1..10000] 
        |> List.fold 
            (fun (monkeys, itemsInspected) i  -> 
                doRun manageWorryLevel monkeys itemsInspected)
            (monkeys, Map.empty)

    let answer = 
        (snd result)
        |> Map.toList
        |> List.map snd
        |> List.sortDescending
        |> List.take 2
        |> List.reduce (*)
    Console.WriteLine $"11b: {answer}"
