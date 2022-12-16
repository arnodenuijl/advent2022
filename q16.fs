module q16

open System
open System.IO
open System.Threading
open utils
open FParsec

let inputString = File.ReadAllText "16.txt"
// let inputString = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
// Valve BB has flow rate=13; tunnels lead to valves CC, AA
// Valve CC has flow rate=2; tunnels lead to valves DD, BB
// Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
// Valve EE has flow rate=3; tunnels lead to valves FF, DD
// Valve FF has flow rate=0; tunnels lead to valves EE, GG
// Valve GG has flow rate=0; tunnels lead to valves FF, HH
// Valve HH has flow rate=22; tunnel leads to valve GG
// Valve II has flow rate=0; tunnels lead to valves AA, JJ
// Valve JJ has flow rate=21; tunnel leads to valve II"""

type ValveName = string

// flow * isOpen * goesTo
type Valve =  int *  bool * ValveName list

type World = Map<ValveName, Valve>

let valveNameParser : Parser<ValveName, unit> = asciiUpper .>>. asciiUpper |>> fun (a1, a2) -> $"{a1}{a2}"
let valveNamesParser : Parser<ValveName list, unit> = sepBy valveNameParser (pstring ", ")

let valveParser
    = skipString "Valve "
             >>. valveNameParser
             .>> skipString " has flow rate="
             .>>. pint32
             .>> choice [
                 skipString "; tunnels lead to valves "
                 skipString "; tunnel leads to valves "
                 skipString "; tunnel leads to valve "
                 skipString "; tunnels leads to valve "
             ]
             .>>. valveNamesParser
             |>> fun ((name, flow), goesTo) -> name, (flow, false, goesTo)
             
             
let nodesParser = sepBy valveParser skipNewline |>> Map.ofSeq
let nodes = run nodesParser inputString |> unwrapParserResult

type Action =
    | Open
    | GoTo of ValveName
    
let possibeFlowPerMinuteForAllClosedIfTheyOpenedNow (world: World) =
    world.Values
    |> Seq.filter (fun (_, isOpen, _) -> not isOpen)
    |> Seq.sumBy (fun (flow, _, _) -> flow)
 
let q16a () =
    let possibleActions ((flow, isOpen, goesTo): Valve) =
        seq {
            if not isOpen && flow > 0 then yield Open
            for next in goesTo do
                yield GoTo next
        } |> List.ofSeq

    let worldStateHash (time: int) (currentValveName: string) (world : World) =
        world
        |> Map.filter (fun _ (_, isOpen, _) ->  isOpen)
        |> Map.keys
        |> Seq.sort
        |> fun names -> String.Join("", names)
        |> fun s -> $"{time}-{currentValveName}-{s}"

    let mutable state = Map.empty<string, int * string list>
     
    let rec doStep (currentTime : int) (currentScore : int) (world : World) (currentValveName : ValveName) (log : string list)=
        let worldStateHash = worldStateHash currentTime currentValveName world
        match Map.tryFind worldStateHash state with
        | Some _ -> Seq.empty
        | None ->
            state <- Map.add worldStateHash (currentScore, log) state
            seq {
                yield (worldStateHash, currentScore)
                if currentTime < 31 then
                        
                    let flow, isOpen, goesTo = world[currentValveName]
                    let possibleActions = possibleActions (flow, isOpen, goesTo)
                    for a in possibleActions do
                        if currentTime = 14 && currentValveName = "EE" && currentScore = 1326 then
                            Console.WriteLine $" -- {possibleActions} ({currentScore})"
                        match a with 
                        | Open ->
                            let addedScore = (30 - currentTime) * flow
                            yield! doStep (currentTime + 1) (addedScore + currentScore) (Map.add currentValveName (flow, true, goesTo) world) currentValveName ($"{currentTime} Open {currentValveName} - {addedScore + currentScore}" :: log)
                        | GoTo next ->
                            yield! doStep (currentTime + 1) currentScore world next ( $"{currentTime} Move to {next} - {currentScore}" :: log)
                        
                } 
    let allPossible  = doStep 1 0 nodes "AA" []
    let mutable highest = 0 
    let mutable highestHash = "" 
    for (hash, score) in allPossible do
       if score > highest then
           highest <- score
           highestHash <- hash
           Console.WriteLine $"{highest} {hash} "
    () 
let q16b () =
    let possibleActions ((flow, isOpen, goesTo): Valve) (previous : Option<ValveName>) =
        seq {
            if not isOpen && flow > 0 then yield Open
            for next in goesTo do
                if previous.IsNone || previous.Value <> next then 
                    yield GoTo next
        } |> List.ofSeq

    let worldStateHash (time: int) (currentValveName: string) (currentElephantValveName : ValveName) (world : World) =
        world
        |> Map.filter (fun _ (_, isOpen, _) ->  isOpen)
        |> Map.keys
        |> Seq.sort
        |> fun names -> String.Join("", names)
        |> fun s -> $"{time}-{currentValveName}-{currentElephantValveName}-{s}"

    let mutable state = Map.empty<string, int * string list>
    let mutable highscore = {| Score = 0 ; Hash = "" |}
    
    let rec doStep
            (currentTime : int)
            (currentScore : int)
            (world : World)
            (currentValveName : ValveName)
            (previousValveName : Option<ValveName>)
            (currentElephantValveName : ValveName)
            (previousElephantValveName : Option<ValveName>)
            (log : string list)=
        let currentStateHash = worldStateHash currentTime currentValveName currentElephantValveName world
        if currentScore > highscore.Score then
            highscore <- {| Score = currentScore; Hash = currentStateHash |}
        
        match Map.tryFind currentStateHash state with
        | Some _ -> Seq.empty
        | None ->
            state <- state
                     |> Map.add currentStateHash (currentScore, log)
                     |> Map.add (worldStateHash currentTime currentElephantValveName currentValveName world) (currentScore, log)
            
            let extraIfAllOpenedNow = (26 - currentTime) * possibeFlowPerMinuteForAllClosedIfTheyOpenedNow world
            let maxTotal = currentScore + extraIfAllOpenedNow
            if maxTotal < highscore.Score then
                Seq.empty
            else
                seq {
                    yield (currentStateHash, currentScore)
                    if currentTime < 27 then                        
                        let flow, isOpen, goesTo = world[currentValveName]
                        let flowElephant, isOpenElephant, goesToElephant = world[currentElephantValveName]
                        
                        let myActions = possibleActions (flow, isOpen, goesTo) previousValveName
                        let elephantActions = possibleActions (flowElephant, isOpenElephant, goesToElephant) previousElephantValveName
                        
                        let combinedActions =
                            myActions
                            |> List.collect (fun myAction ->
                                elephantActions
                                |> List.collect (fun eAction ->
                                        if  Open = myAction && Open =  eAction && currentValveName = currentElephantValveName then
                                            []
                                        else
                                            [(myAction, eAction)]
                                    ))
                        
                        for myAction,eAction in combinedActions do
                            let myNewPos =
                                match myAction with
                                | GoTo next -> next
                                | _ -> currentValveName
                            
                            let elephantNewPos =
                                match eAction with
                                | GoTo next -> next
                                | _ -> currentElephantValveName
                            
                            let updatedScore =
                                match myAction,eAction with
                                | Open, _ -> currentScore + (26 - currentTime) * flow
                                | _, Open -> currentScore + (26 - currentTime) * flowElephant
                                | _,_ -> currentScore
                            let myPrev =
                                match myAction with
                                | Open -> None
                                | GoTo x -> Some(x)
                            let elPrev =
                                match myAction with
                                | Open -> None
                                | GoTo x -> Some(x)
                            
                            let updatedWorld =
                                world
                                |> Map.add currentValveName (flow, isOpen || myAction = Open, goesTo)
                                |> Map.add currentElephantValveName (flowElephant, isOpenElephant || eAction = Open, goesToElephant)
                                
                            yield! doStep
                                       (currentTime + 1)
                                       updatedScore
                                       updatedWorld
                                       myNewPos
                                       myPrev
                                       elephantNewPos
                                       elPrev
                                       ($"{currentTime} Open {currentValveName} - {updatedScore}" :: log)                        
                    } 
    let allPossible  = doStep 1 0 nodes "AA" None "AA" None []
    let mutable highest = 0 
    let mutable highestHash = "" 
    for (hash, score) in allPossible do
       if score > highest then
           highest <- score
           highestHash <- hash
           Console.WriteLine $"{highest} {hash} "
