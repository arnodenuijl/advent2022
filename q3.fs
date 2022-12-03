module q3

open System
open System.IO

type Bag = {
    Left : Set<char>
    Right : Set<char>
}

let rawInput = 
    File.ReadAllLines "03.txt"

let createBag (s:string) = 
    let left = 
        s.Substring(0, s.Length / 2)
        |> Set.ofSeq
    let right = 
        s.Substring(s.Length / 2, s.Length / 2)
        |> Set.ofSeq
    { Bag.Left = left
      Bag.Right = right }

let getCommonItemInBothSides (bag: Bag) =
    Set.intersect bag.Left bag.Right
    |> Seq.exactlyOne

let calculatePriority (c:char) : int =
    if c >= 'a' && c <= 'z' then
        (int c) - (int 'a') + 1 
    else if c >= 'A' && c <= 'Z' then
        (int c) - (int 'A') + 27
    else failwith $"kan geen priority bepalen voor {c}"

let getBadge (bags: Bag array) =
    bags
    |> Seq.map (fun b -> Set.union b.Left b.Right)
    |> Set.intersectMany
    |> Seq.exactlyOne

let q3a () = 
    let result = 
        rawInput
        |> Array.map createBag
        |> Array.map getCommonItemInBothSides
        |> Array.map calculatePriority
        |> Array.sum

    Console.WriteLine $"3a: {result}"

let q3b () =
    let result = 
        rawInput
        |> Array.map createBag
        |> Array.chunkBySize 3
        |> Array.map getBadge
        |> Array.map calculatePriority
        |> Array.sum

    Console.WriteLine $"3b: {result}"
