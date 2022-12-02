module q1

open System
open System.IO

let elvesCalories = 
    File.ReadAllText "01.txt"
    |> fun text -> text.Split("\r\n\r\n", StringSplitOptions.RemoveEmptyEntries)
    |> Array.map 
        (fun textBlockForElve -> 
            textBlockForElve.Split("\r\n")
            |> Array.map int
            |> Array.sum)

let q1a () =
    let maxElve = 
        elvesCalories
        |> Array.max
    Console.WriteLine $"1a: {maxElve}"

let q1b () =
    let top3ElvesTotal = 
        elvesCalories
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum
    Console.WriteLine $"1b: {top3ElvesTotal}"