module q18

open System
open System.IO
open System.Runtime.Intrinsics.X86
open System.Threading
open utils
open FParsec

type Coord = int*int*int

type SurfacePoints = Coord list
let input = File.ReadAllText "18.txt"

let allBlocks =
    input.Split(Environment.NewLine)
    |> Array.map (fun line ->
        line.Split(",")
        |> fun [|x;y;z|] -> (int x, int y, int z) )
    |> List.ofArray
    
let baseBlockSides = [
    [(0,0,0);(1,0,0);(1,1,0);(0,1,0)] 
    [(0,0,1);(1,0,1);(1,1,1);(0,1,1)] 
    [(1,0,0);(1,1,0);(1,1,1);(1,0,1)]
    [(0,0,0);(0,1,0);(0,1,1);(0,0,1)]
    [(0,0,0);(1,0,0);(1,0,1);(0,0,1)]
    [(0,1,0);(1,1,0);(1,1,1);(0,1,1)]]

let blockToSurfacePoints ((blockX,blockY,blockZ) : int*int*int): (int * int * int) list list =
    baseBlockSides
    |> List.map (fun side ->
        side
        |> List.map (fun (x,y,z) -> (blockX + x, blockY + y, blockZ + z))
        |> List.sort) // sort to make it possible to filter out duplicates

let removeDuplicateSurfacePoints (items : SurfacePoints list ): SurfacePoints list =
    items
    |> List.fold (fun (remaining, dupes) listItem ->
            if List.contains listItem dupes then
                (remaining, dupes)
            else
                match List.tryFindIndex ((=) listItem) remaining with
                | Some index ->
                    (List.removeAt index remaining, listItem :: dupes)
                | None ->
                    (listItem :: remaining, dupes)
            )
            ([], [])
    |> fst

let allBlockSurfaces =
    allBlocks
    |> List.collect blockToSurfacePoints
    |> removeDuplicateSurfacePoints
let getAdjecentBlocks ((bx,by,bz) : int*int*int) =
    [
      (bx - 1, by, bz)
      (bx + 1, by, bz)
      (bx, by - 1, bz)
      (bx, by + 1, bz)
      (bx, by, bz - 1)
      (bx, by, bz + 1)
    ]
      
let q18a () =
    let result =
        allBlockSurfaces
        |> List.length
    Console.WriteLine $"18a: {result}"

let q18b () =
    let minX, maxX, minY, maxY, minZ, maxZ =
        allBlocks
        |> List.fold
            (fun (minX, maxX, minY, maxY, minZ, maxZ) (x,y,z) ->
                (min minX x, max maxX x, min minY y, max maxY y , min minZ z, max maxZ z) )
            (Int32.MaxValue, Int32.MinValue ,Int32.MaxValue ,Int32.MinValue ,Int32.MaxValue ,Int32.MinValue )
        |> fun (minX, maxX, minY, maxY, minZ, maxZ) -> (minX - 1, maxX + 1, minY - 1, maxY + 1, minZ - 1, maxZ + 1)
    Console.WriteLine $"Flood water from {minX},{minY},{minZ} to {maxX}{maxY},{maxZ}"
        
    let blocksSet = allBlocks |> Set.ofSeq
    
    let rec flood (water : Set<int*int*int>) =
        let addedWater =
            water
            |> Seq.collect getAdjecentBlocks
            |> Seq.filter (fun (x,y,z) ->
               x >= minX && x <= maxX &&
               y >= minY && y <= maxY &&
               z >= minZ && z <= maxZ)         
            |> Set.ofSeq            
            |> fun adjecentBlocks -> Set.difference adjecentBlocks water
            |> fun adjecentBlocks -> Set.difference adjecentBlocks blocksSet
        let updatedWater = Set.union addedWater water
        if updatedWater.Count = water.Count then updatedWater
        else flood updatedWater
    
    let allWater =
        flood (Set.singleton (minX, minY, minZ))
        |> Set.toList
    Console.WriteLine $"{allWater.Length} water"
    
    let allWaterSurfaces =
        allWater
        |> List.collect blockToSurfacePoints
        |> removeDuplicateSurfacePoints
    Console.WriteLine $"{allWaterSurfaces.Length} water surfaces"

    let touchingSurfaces =
        allBlockSurfaces
        |> Set.ofList
        |> Set.intersect (Set.ofList allWaterSurfaces)
    Console.WriteLine $"18b: {touchingSurfaces.Count}"
    ()
    
    