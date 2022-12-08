module q8

open System
open System.IO
open utils

let input = File.ReadAllLines "08.txt"

let data = Array2D.create (input.[0].Length) input.Length 0
input 
|> Seq.iteri (fun y line -> 
    line 
    |> Seq.iteri (fun x c -> 
            data[x,y] <- charToInt c ))

let scanHorizontal y xRange = 
    seq {
        let mutable highest = -1
        for x in xRange do      
            if data[x,y] > highest then 
                yield (x,y)
                highest <- data[x,y]
    }
    
let scanVertical x yRange = 
    seq {
        let mutable highest = -1
        for y in yRange do        
            if data[x,y] > highest then 
                yield (x,y)
                highest <- data[x,y]
    }

let xLength = Array2D.length1 data
let yLength = Array2D.length2 data

let topToBottomRange = seq {0 .. 1 .. (yLength-1)} |> List.ofSeq 
let bottomToTopRange = seq {(yLength-1) .. -1 .. 0} |> List.ofSeq
let leftToRightRange = seq {0 .. 1 .. (xLength-1)} |> List.ofSeq
let rightToLeftRange = seq {(xLength-1) .. -1 .. 0} |> List.ofSeq

let q8a () =

    let leftToRight = 
        topToBottomRange
        |> Seq.collect (fun y -> scanHorizontal y leftToRightRange)
        |> Set.ofSeq
    let rightToLeft = 
        topToBottomRange
        |> Seq.collect (fun y -> scanHorizontal y rightToLeftRange)
        |> Set.ofSeq
    let topToBottem = 
        leftToRightRange
        |> Seq.collect (fun y -> scanVertical y topToBottomRange)
        |> Set.ofSeq
    let bottomToTop = 
        leftToRightRange
        |> Seq.collect (fun y -> scanVertical y bottomToTopRange)
        |> Set.ofSeq

    let allPoints =
        Set.unionMany [leftToRight;rightToLeft;topToBottem;bottomToTop]
    
    Console.WriteLine $"8a: {Set.count allPoints}"

let q8b() =
    let viewingDistance (height : int) (lineOfSight : int array) =
        let mutable blocked = false
        seq {
            for t in lineOfSight do
                if not blocked then
                    yield t
                    if t >= height then 
                        blocked <- true
        }
        |> Seq.length

    let viewingDistanceAllWays x y =
        let height = data[x,y]

        let leftLineOfSight = data[..x,y] |> Array.rev |> Array.skip 1
        let rightLineOfSight = data[x..,y] |> Array.skip 1
        let upLineOfSight = data[x,..y] |> Array.rev |> Array.skip 1
        let downLineOfSight = data[x,y..] |> Array.skip 1

        (viewingDistance height leftLineOfSight) 
            * (viewingDistance height rightLineOfSight) 
            * (viewingDistance height upLineOfSight) 
            * (viewingDistance height downLineOfSight) 

    let allViewingDistances =
        seq {
            for x in leftToRightRange do
                for y in topToBottomRange do 
                    viewingDistanceAllWays x y                    
        }
    let test = allViewingDistances |> Seq.max  
    Console.WriteLine $"8b: {test}"