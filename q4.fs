module q4

open System
open System.IO

type Section = {
    Start : int
    End : int
}

let getSectionFromString (s:string) =
    s.Split("-") 
    |> Array.map int
    |> fun numbers -> 
        { Section.Start = numbers[0] 
          Section.End = numbers[1] }

let getSectionPairsFromString (s:string) = 
    s.Split(",") 
    |> List.ofArray
    |> fun items -> (getSectionFromString items[0], getSectionFromString items[1])

let isFullyContained ( (firstSection, secondSection) : Section * Section) =
    (firstSection.Start <= secondSection.Start && firstSection.End >= secondSection.End) ||
    (firstSection.Start >= secondSection.Start && firstSection.End <= secondSection.End)

let isPartiallyContained  ( (firstSection, secondSection) : Section * Section) =
    (firstSection.Start <= secondSection.End && firstSection.End >= secondSection.Start) ||
    (secondSection.Start <= firstSection.End && secondSection.End >= firstSection.Start)

let sectionsList = 
    File.ReadAllLines "04.txt"
    |> Array.map getSectionPairsFromString

let q4a () =
    let result = 
        sectionsList
        |> Array.filter isFullyContained
        |> Array.length
    Console.WriteLine $"4a: {result}"
    
let q4b () =
    let result = 
        sectionsList
        |> Array.filter isPartiallyContained
        |> Array.length
    Console.WriteLine $"4b: {result}"
    