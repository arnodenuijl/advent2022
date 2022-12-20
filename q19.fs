module q19

open System
open System.Collections.Generic
open System.IO
open FSharp.Collections.ParallelSeq
open utils
open FParsec

// let input = """1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
// 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
let input = File.ReadAllText "19.txt"


type Blueprint = {
    BlueprintNumber : int
    OreRobotPrice : int
    ClayRobotPrice : int
    ObsidianRobotPriceOre : int
    ObsidianRobotPriceClay : int
    GeodeRobotPriceOre : int  
    GeodeRobotPriceObsidian : int 
}

let lineParser =
    skipString "Blueprint " >>.
    pint32 .>> skipString ": Each ore robot costs " .>>.
    pint32 .>> skipString " ore. Each clay robot costs " .>>.
    pint32 .>> skipString " ore. Each obsidian robot costs " .>>.
    pint32 .>> skipString " ore and " .>>.
    pint32 .>> skipString " clay. Each geode robot costs " .>>.
    pint32 .>> skipString " ore and " .>>.
    pint32 .>> skipString " obsidian."
    |>> fun ((((((blueprintNr, oreRobotCost), clayRobotCost), obsidianRobotOreCost),obsidianRobotClayCost), geodeOreCost), geodeObsidianCost) ->
        { BlueprintNumber = blueprintNr
          OreRobotPrice = oreRobotCost
          ClayRobotPrice = clayRobotCost
          ObsidianRobotPriceOre = obsidianRobotOreCost
          ObsidianRobotPriceClay = obsidianRobotClayCost 
          GeodeRobotPriceOre = geodeOreCost 
          GeodeRobotPriceObsidian = geodeObsidianCost }
let blueprintsParser = sepBy lineParser skipNewline

Console.WriteLine input
let blueprints = run blueprintsParser input |> unwrapParserResult

type Action =
| Nothing
| BuyOreRobot
| BuyClayRobot
| BuyObsidianRobot
| BuyGeodeRobot 

type Stock = {
    Ore : int
    Clay : int
    Obsidian : int
    Geode : int
    OreRobots : int
    ClayRobots : int
    ObsidianRobots : int
    GeodeRobots : int
}

let startStock = {
    Ore=0
    Clay=0
    Obsidian=0
    Geode=0
    OreRobots=1
    ClayRobots=0
    ObsidianRobots=0
    GeodeRobots=0
}

let getValidActions (blueprint: Blueprint) (stock: Stock) =
    seq {
        yield Nothing
        if stock.Ore >= blueprint.OreRobotPrice then yield BuyOreRobot
        if stock.Ore >= blueprint.ClayRobotPrice then yield BuyClayRobot
        if stock.Ore >= blueprint.ObsidianRobotPriceOre &&
           stock.Clay >= blueprint.ObsidianRobotPriceClay then yield BuyObsidianRobot
        if stock.Ore >= blueprint.GeodeRobotPriceOre &&
           stock.Obsidian >= blueprint.GeodeRobotPriceObsidian then yield BuyGeodeRobot
    } |> List.ofSeq

let payAction (blueprint: Blueprint)(action: Action ) (stock: Stock) : Stock =
    match action with
    | Nothing -> stock
    | BuyOreRobot -> { stock with Ore = stock.Ore - blueprint.OreRobotPrice }
    | BuyClayRobot -> { stock with Clay = stock.Clay - blueprint.ClayRobotPrice }
    | BuyObsidianRobot -> { stock with
                                Ore = stock.Ore - blueprint.ObsidianRobotPriceOre
                                Clay = stock.Clay - blueprint.ObsidianRobotPriceClay }
    | BuyGeodeRobot -> { stock with
                                Ore = stock.Ore - blueprint.GeodeRobotPriceOre
                                Obsidian = stock.Obsidian - blueprint.GeodeRobotPriceObsidian }

let doAction (action: Action) (stock: Stock) : Stock =     
    match action with
    | Nothing -> stock
    | BuyOreRobot -> { stock with OreRobots = stock.OreRobots + 1 }
    | BuyClayRobot -> { stock with ClayRobots = stock.ClayRobots + 1 }
    | BuyObsidianRobot -> { stock with ObsidianRobots = stock.ObsidianRobots + 1 }
    | BuyGeodeRobot -> { stock with GeodeRobots = stock.GeodeRobots + 1 }
    
let calculateQualityLevel (blueprint: Blueprint) (stock: Stock) =
    stock.Geode * blueprint.BlueprintNumber


let harvest (stock: Stock): Stock = { stock with
                                        Ore = stock.Ore + stock.OreRobots
                                        Clay = stock.Clay + stock.ClayRobots
                                        Obsidian = stock.Obsidian + stock.ObsidianRobots
                                        Geode = stock.Geode + stock.GeodeRobots
                                     }
let  calculateBlueprint (blueprint : Blueprint) =
    let seenStates = HashSet()

    let rec doStep (stepNumber : int) (blueprint : Blueprint) (stock: Stock) (action : Action) (log: Action list) =
        if stepNumber = 25 then [blueprint, stock, log]
        else if seenStates.Contains (stepNumber, stock, action)  then []
        else if stock.ClayRobots > blueprint.ObsidianRobotPriceClay then []
        else if stock.OreRobots > blueprint.ClayRobotPrice &&
                stock.OreRobots > blueprint.GeodeRobotPriceOre &&
                stock.OreRobots > blueprint.ObsidianRobotPriceOre then []
        else if stock.ObsidianRobots > blueprint.GeodeRobotPriceObsidian then []
        else
            seenStates.Add (stepNumber, stock, action) |> ignore
            let updatedStock =
                stock
                |> payAction blueprint action
                |> harvest
                |> doAction action
                
            let possibleActions = getValidActions blueprint stock
            possibleActions
            // |> List.collect (fun a -> doStep (stepNumber + 1) blueprint updatedStock a (action :: log))
            |> List.collect (fun a -> doStep (stepNumber + 1) blueprint updatedStock a [])
    doStep 1 blueprint startStock Nothing []
let q19a () =
    Console.WriteLine $"{blueprints.Length} blueprints"
    let result =
        blueprints
        |> PSeq.map ( fun b ->
            calculateBlueprint b
            |> List.sortByDescending (fun (blueprint, stock, log) -> stock.Geode)
            |> List.head
            |> fun (bp, stock, actions) -> bp.BlueprintNumber * stock.Geode  )
        |> PSeq.sum
    Console.WriteLine $"19a: {result}"
let q19b () =
    ()