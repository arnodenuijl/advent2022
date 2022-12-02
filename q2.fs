module q2

open System
open System.IO

type Move = 
| Rock
| Paper
| Scissors

let calculateScores (games: (Move * Move)[]) =
    let calculateGameScore (game: (Move * Move)) =
        let result = 
            match game with 
            | Rock , Rock -> 3
            | Rock , Paper -> 6
            | Rock , Scissors -> 0
            | Paper , Rock -> 0
            | Paper , Paper -> 3
            | Paper , Scissors -> 6
            | Scissors , Rock -> 6
            | Scissors , Paper -> 0
            | Scissors , Scissors -> 3
        result

    let calculateMoveScore (game: (Move * Move)) =
        let result = 
            match snd game with 
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3
        result

    games 
    |> Array.map (fun g -> 
        let moveScore = calculateMoveScore g
        let gameScore = calculateGameScore g
        let total = moveScore + gameScore
        // Console.WriteLine $"{g} : {moveScore} + {gameScore} = {total}"
        total
        )
    |> Array.sum

module q2a =
    // A -> Rock         X -> Rock
    // B -> Paper        Y -> Paper
    // C -> Scissors     Z -> Scissors
    let toMove (s:string) = 
        match s with 
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors
        | _ -> failwith $"{s} is geen geldige move"

    let q2a () = 
        let games: (Move * Move)[] = 
            File.ReadAllLines("02.txt")
            |> Array.map (fun s -> s.Split(" "))
            |> Array.map (fun ss -> (toMove ss.[0], toMove ss.[1]))
            
            
        let scores = calculateScores games

        Console.WriteLine $"2a: Totaal: {scores} (from {games.Length} scores)"

module q2b =
    type GameResult = 
    | Lose
    | Draw
    | Win

    let toMove (s:string) = 
        match s with 
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors
        | _ -> failwith $"{s} is geen geldige move"

    let toGameResult (s:string) = 
        match s with 
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win
        | _ -> failwith $"{s} is geen geldige move"


    let q2b () = 
        
        let calculateMoveFromResult (otherMove: Move) (gameResult: GameResult) : Move = 
            match otherMove, gameResult with
            | Rock, Lose -> Scissors
            | Rock, Draw -> Rock
            | Rock, Win -> Paper
            | Paper, Lose -> Rock
            | Paper, Draw -> Paper
            | Paper, Win -> Scissors
            | Scissors, Lose -> Paper
            | Scissors, Draw -> Scissors
            | Scissors, Win -> Rock
        
        let gamesAndResult: (Move * GameResult)[] = 
            File.ReadAllLines("02.txt")
            |> Array.map (fun s -> s.Split(" "))
            |> Array.map (fun ss -> (toMove ss.[0], toGameResult ss.[1]))
            
        let games = 
            gamesAndResult 
            |> Array.map (fun (otherMove, gameResult) -> (otherMove, calculateMoveFromResult otherMove gameResult))

        let scores = calculateScores games

        Console.WriteLine $"2b: Totaal: {scores} (from {games.Length} scores)"
