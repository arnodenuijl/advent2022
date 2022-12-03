module utils

open System
open System.IO

let allPairs (items : 'a seq) =
    let list = List.ofSeq items
    let listLength = Seq.length list

    seq {
        for i = 0 to listLength - 1  do
            for j = 0 to listLength - 1 do
                if i <> j then
                    yield (list[i], list[j])  
    }
