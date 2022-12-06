module q6

open System
open System.IO

let input = File.ReadAllText "06.txt"

let q6a() = 
    input
    |> Seq.windowed 4
    |> Seq.mapi(fun i item -> (i + 4, item))
    |> Seq.filter (fun (i, item) -> 
        item
        |> Set.ofSeq
        |> fun s -> s.Count = 4 )
    |> Seq.head
    |> fun x -> Console.WriteLine $"{fst x}"

let q6b() = 
    input
    |> Seq.windowed 14
    |> Seq.mapi(fun i item -> (i + 14, item))
    |> Seq.filter (fun (i, item) -> 
        item
        |> Set.ofSeq
        |> fun s -> s.Count = 14 )
    |> Seq.head
    |> fun x -> Console.WriteLine $"{fst x}"    