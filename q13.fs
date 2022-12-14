module q13

open System
open System.IO
open utils
open FParsec

let inputString = File.ReadAllText "13.txt"

type Packet = 
| PacketInt of int
| PacketList of Packet list

module Parsing =
    let packetListParser, packetListParserRef = createParserForwardedToRef<Packet, unit>()
    
    let packetIntParser = pint32 |>> PacketInt
    let intOrListParser = choice [packetIntParser;packetListParser]
    packetListParserRef.Value <- between (pstring "[") (pstring "]") (sepBy intOrListParser (pstring ",")) |>> PacketList

    let packetPairParser = packetListParser .>> skipNewline .>>. packetListParser
    let packetPairsParser = sepBy packetPairParser (skipNewline >>. skipNewline)

let input = run Parsing.packetPairsParser inputString |> unwrapParserResult

type CompareResult = 
| RightOrder
| WrongOrder
| Undecided

let rec compareList (l1: Packet list) (l2 : Packet list) =
    match l1, l2 with 
    | h1 :: t1, h2 :: t2 -> 
        match compare h1 h2 with
        | RightOrder -> RightOrder
        | WrongOrder -> WrongOrder
        | Undecided ->  compareList t1 t2
    | [], _ :: _ -> RightOrder
    | _ :: _ , []-> WrongOrder
    | [], [] -> Undecided
    
and  compare (p1: Packet) (p2: Packet) =
    match p1, p2 with
    | PacketInt i1, PacketInt i2 -> 
        if i1 < i2 then RightOrder
        else if i1 > i2 then WrongOrder
        else Undecided
    | PacketList _, PacketInt i2 -> compare p1 (PacketList [PacketInt i2])
    | PacketInt i1, PacketList _ -> compare (PacketList [PacketInt i1]) p2
    | PacketList l1, PacketList l2 -> compareList l1 l2

let rec printPacket (p:Packet) : string=
    match p with
    | PacketInt p -> p.ToString()
    | PacketList ps -> 
        let itemsString = 
            ps 
            |> List.map printPacket
            |> List.toArray
            |> fun items -> String.Join("," , items) 
        $"[{itemsString}]"

let q13a () =
    let all = 
        input
        |> List.mapi (fun i (p1, p2) -> (i+1, compare p1 p2))
    
    let result = 
        all
        |> List.filter (fun (_, r) -> 
                            match r with
                            | RightOrder -> true
                            | _ -> false )
        |> List.map fst
        |> List.sum
    Console.WriteLine $"13a: {result}"

let q13b () =
    let divider1 = PacketList [PacketList [PacketInt 2]]
    let divider2 = PacketList [PacketList [PacketInt 6]]

    let allPackets = 
        divider1 :: divider2 :: (input |> List.collect (fun (p1,p2) -> [p1;p2]))
    
    let sorted = 
        allPackets
        |> List.sortWith (fun p1 p2 -> 
            match compare p1 p2 with
            | RightOrder -> -1
            | WrongOrder -> 1
            | Undecided -> 0)

    let index1 = (List.findIndex (fun x -> x = divider1) sorted) + 1
    let index2 = (List.findIndex (fun x -> x = divider2) sorted) + 1
    Console.WriteLine $"13b: {index1 * index2}"