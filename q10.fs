module q10

open System
open System.IO
open utils
open FParsec

let input = File.ReadAllText "10.txt"

type Command =
| AddX of int
| Noop

type State = int * int

let commandParser = choice [
    pstring "addx" >>. spaces >>. pint32 |>> AddX
    pstring "noop" |>> fun _ -> Noop
]

let commandsToStates (cmds: Command list) =
    cmds
    |> List.fold (fun (acc: (int*int) list) (cmd: Command) -> 
            let (previousCycle: int, previousValue: int) = acc[acc.Length - 1]
            match cmd with 
            | Noop -> acc @ [(previousCycle + 1, previousValue)]
            | AddX (x: int) -> acc @ [(previousCycle + 2, previousValue + x)]
            )
            [(1,1)]

let getValueAddCycle (cycle : int) (states : State list) =
    let index: int = 
        states
        |> List.findIndex (fun (c, state) -> c > cycle)
    states[index - 1]
    |> snd

let q10a() = 
    let cmds: Command list = run (sepBy commandParser newline) input |> unwrapParserResult
    let states: (int * int) list = commandsToStates cmds

    let value20: int = getValueAddCycle 20 states
    let value60: int = getValueAddCycle 60 states
    let value100: int = getValueAddCycle 100 states
    let value140: int = getValueAddCycle 140 states
    let value180: int = getValueAddCycle 180 states
    let value220: int = getValueAddCycle 220 states

    let result = 
        20 * value20 + 
        60 * value60 + 
        100 * value100 + 
        140 * value140 + 
        180 * value180 + 
        220 * value220
    Console.WriteLine $"10a: {result}"
    
let q10b() = 
    let cmds: Command list = run (sepBy commandParser newline) input |> unwrapParserResult
    let states : State list = commandsToStates cmds
    let expanded : State list = 
        [1..240]
        |> List.map (fun i -> (i, getValueAddCycle i states))

    let litPixels =
        seq {
            for (clock, spritePosition) in expanded do 
                let posX = (clock - 1) % 40
                if abs (posX - spritePosition) <= 1 then
                    yield '#'
                else yield '.'
        } 
        |> Array.ofSeq
        |> String
        
    Console.WriteLine $"10b:"
    litPixels
    |> Seq.chunkBySize 40
    |> Seq.iter (fun s -> 
        Console.WriteLine $"{s |> Array.ofSeq |> String}")
