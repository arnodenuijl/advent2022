module q11

open System
open System.IO
open utils
open FParsec

let input = File.ReadAllText "11.txt"
let testInput = """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""

type Operation = 
| Add of uint64
| Multiply of uint64
| Pow2


type Monkey = {
    Items : uint64 list
    DivideWorryByThree : bool
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
    let startingItemsParser = spaces >>. pstring "Starting items: " >>. sepBy puint64 (pstring ", ")
    let operationParser = 
        spaces >>. choice [
            spaces >>. pstring "Operation: new = old * old" |>> fun _ -> Pow2
            spaces >>. pstring "Operation: new = old + " >>. puint64 |>> Add
            spaces >>. pstring "Operation: new = old * " >>. puint64 |>> Multiply
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
                                Monkey.DivideWorryByThree = true
                                }) 
    let monkeysParser = sepBy monkeyParser (skipNewline >>. skipNewline)                      

let doOperation (operation: Operation) (inputValue : uint64) =
    match operation with 
    | Pow2 -> inputValue * inputValue
    | Add x -> inputValue + x
    | Multiply x -> inputValue * x 

let createMonkeyActions (monkey: Monkey) = 
    let actions = 
        monkey.Items
        |> List.map (fun item -> 

                        let newWorry = 
                            if monkey.DivideWorryByThree then
                                (doOperation monkey.Operation item) / (uint64 3)
                            else
                                (doOperation monkey.Operation item)

                        if newWorry % monkey.DivisorTest  = (uint64 0) then  
                            { MonkeyAction.ToMonkey =monkey.MonkeyThrowTrue
                              MonkeyAction.Item = newWorry}
                        else 
                            { MonkeyAction.ToMonkey =monkey.MonkeyThrowFalse
                              MonkeyAction.Item = newWorry}
                        )
    actions

    
let processMonkey (monkeyNuber:int) (monkeys : Map<int, Monkey>) (itemsIspected : Map<int, int>)=
    let updatedIemsInspected = 
        itemsIspected
        |> Map.change monkeyNuber (fun inspectedOption -> 
                                            let currentCount = Option.defaultValue 0 inspectedOption
                                            currentCount + monkeys[monkeyNuber].Items.Length
                                            |> Some)

    let updatedMonkeys = 
        let actions = createMonkeyActions monkeys[monkeyNuber]
        actions
        |> List.fold
            (fun (ms:Map<int, Monkey>) (action: MonkeyAction) -> 
                let monkeyToThrowTo = ms[action.ToMonkey]
                let updateMonkeyToThrowTo = { monkeyToThrowTo with Items = monkeyToThrowTo.Items @ [action.Item] }
                
                ms 
                |> Map.add action.ToMonkey updateMonkeyToThrowTo
            )
            monkeys
        
        |> Map.add monkeyNuber ({monkeys[monkeyNuber] with Items = []}) 
    (updatedMonkeys, updatedIemsInspected)

let doRun (monkeys : Map<int, Monkey>) (monkeysInspected: Map<int,int>)= 
    [0..(monkeys.Count - 1)]
    |> List.fold 
                (fun (monkeys, itemsInspected) monkeyNumberToProcess -> 
                        processMonkey monkeyNumberToProcess monkeys itemsInspected)
                (monkeys, monkeysInspected)

let printMonkeys (monkeys : Map<int, Monkey>) = 
    [0..(monkeys.Count - 1)]
    |> List.iter (fun i -> 
        let m = monkeys[i]
        let items = String.Join(", " , m.Items)
        Console.WriteLine $"{i}: {items}"
        )
let printItemsInspected (itemsInspected: Map<int,int>) = 
    [0..(itemsInspected.Count - 1)]
    |> List.iter (fun i -> 
        Console.WriteLine $"{i} {itemsInspected[i]}"
    )

let q11a() = 
    let monkeys = run Parsing.monkeysParser input |> unwrapParserResult |> Map.ofList
    monkeys 
    |> Map.iter (fun nr m -> Console.WriteLine $"{nr}: {m}")
        
    let result = 
        [1..20] 
        |> List.fold 
            (fun (monkeys, itemsInspected) i  -> doRun monkeys itemsInspected)
            (monkeys, Map.empty)
    printMonkeys (fst result)
    printItemsInspected (snd result)
    let answer = 
        (snd result)
        |> Map.toList
        |> List.map snd
        |> List.sortDescending
        |> List.take 2
        |> List.reduce (*)
    Console.WriteLine $"11a: {answer}"
    ()  

let q11b() = 
    let monkeys = 
        run Parsing.monkeysParser testInput |> unwrapParserResult 
        |> Map.ofList
        |> Map.map (fun _ m -> {m with DivideWorryByThree = false})

    let result = 
        [1..10000] 
        |> List.fold 
            (fun (monkeys, itemsInspected) i  -> doRun monkeys itemsInspected)
            (monkeys, Map.empty)
    printMonkeys (fst result)
    printItemsInspected (snd result)
    let answer = 
        (snd result)
        |> Map.toList
        |> List.map snd
        |> List.sortDescending
        |> List.take 2
        |> List.reduce (*)
    Console.WriteLine $"11b: {answer}"



    ()