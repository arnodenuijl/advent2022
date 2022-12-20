module q20

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.Intrinsics.X86
open System.Threading
open utils
open FParsec

// let input = """1
// 2
// -3
// 3
// -2
// 0
// 4"""
let input = File.ReadAllText "20.txt"
type ListItem = {
    number : int
    value : bigint
    moved: bool
}

let items: ListItem list =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> List.ofArray
    |> List.mapi (fun i s -> { number = i + 1;value = int s; moved = false })
    
let move i items =
    let currentIndex =
        items
        |> List.findIndex (fun (item : ListItem)-> item.number = i)
    let item = items[currentIndex]
    let listWithoutItem = items |> List.removeAt currentIndex
    let calculatedUpdatedIndex = (((bigint currentIndex + item.value) % (bigint listWithoutItem.Length)) + (bigint listWithoutItem.Length)) % (bigint listWithoutItem.Length)
    let updatedIndex =
        if calculatedUpdatedIndex = bigint 0
        then listWithoutItem.Length
        else int calculatedUpdatedIndex
            
    listWithoutItem
    |> List.insertAt updatedIndex item
let q20a () =
    let endList =
        [1 .. items.Length]
        |> List.fold
               (fun listItems i -> move i listItems)
               items
    let indexOfZero =
        endList
        |> List.findIndex (fun item -> item.value = bigint 0)

    let index1000 = (indexOfZero + 1000) % endList.Length
    let index2000 = (indexOfZero + 2000) % endList.Length
    let index3000 = (indexOfZero + 3000) % endList.Length
    let v1000 = endList[index1000].value
    let v2000 = endList[index2000].value
    let v3000 = endList[index3000].value
    Console.WriteLine $"20a: {v1000} + {v2000} + {v3000} = {v1000 + v2000 + v3000}"

let q20b () =
    let bigItems =
        items
        |> List.map (fun x -> { x with value = x.value * (bigint 811589153) })
    
    let mix (items : ListItem list) = 
        [1 .. items.Length]
        |> List.fold
               (fun listItems i -> move i listItems)
               items
               
    let endList =
        [1..10]
        |> List.fold
            (fun acc _ -> mix acc)
            bigItems
            
    let values =
        endList
        |> List.map (fun x -> x.value)
        |> fun xs -> String.Join(", " , xs)

    let indexOfZero =
        endList
        |> List.findIndex (fun item -> item.value = bigint 0)

    let index1000 = (indexOfZero + 1000) % endList.Length
    let index2000 = (indexOfZero + 2000) % endList.Length
    let index3000 = (indexOfZero + 3000) % endList.Length
    let v1000 = endList[index1000].value
    let v2000 = endList[index2000].value
    let v3000 = endList[index3000].value
    Console.WriteLine $"20b: {v1000} + {v2000} + {v3000} = {v1000 + v2000 + v3000}"
