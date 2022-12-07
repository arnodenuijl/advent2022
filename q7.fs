module q7

open System
open System.IO
open FParsec
open utils

let input = File.ReadAllText "07.txt"

type FileSize = int
type Command = 
| Ls
| GoDirectoryUp
| GoToRootDirectory
| GoToDirectory of string
| FoundSubDir of string
| FoundFile of FileSize * string

module Parser = 
    let goDirectoryUpCommandsParser =
        pstring "$ cd .." >>. skipNewline 
        |>> fun _ -> GoDirectoryUp

    let goToRootDirectoryParser =
        pstring "$ cd /" >>. skipNewline
        |>> fun  _ -> GoToRootDirectory

    let goToDirectoryParser =
        pstring "$ cd " >>. (restOfLine true)
        |>> GoToDirectory

    let foundFileParser = 
        (pint32 .>> spaces .>>. (restOfLine true)) 
        |>> FoundFile

    let foundSubDirParser = 
        pstring "dir" >>. spaces >>. (restOfLine true)
        |>> FoundSubDir

    let lsParser =
        skipString "$ ls" >>. skipNewline
        |>> fun _ -> Ls

    let inputParser = many (choice [
        goToRootDirectoryParser
        goDirectoryUpCommandsParser
        goToDirectoryParser
        foundFileParser
        foundSubDirParser
        lsParser])

type DirName = string
type FileName = string

type Node = 
| DirectoryNode of DirName * Node list
| FileNode of FileName * FileSize

let nodeName n =
        match n with 
        | DirectoryNode (n, _) -> n 
        | FileNode (n, _) -> n

let rec traverse node =
    seq {
        match node with
        | FileNode _ -> yield node  
        | DirectoryNode(_, nodes) -> 
            yield node
            for n in nodes do
                yield! traverse n
    }

let rec add (path : string list) (nodeToAdd : Node) (nodeToAddTo : Node) =
    match path, nodeToAddTo with 
    | _, FileNode(_, _) -> failwith "can't add something to a file node"
    | [], DirectoryNode(dirName, nodes) -> 
        DirectoryNode(dirName, nodeToAdd :: nodes )
    | subNodeName :: restPath, DirectoryNode(dirName, nodes) ->
        let node, rest = 
            nodes 
            |> findWithRest (fun node -> nodeName node = subNodeName)
        let updatedNode = add restPath nodeToAdd node
        DirectoryNode(dirName, updatedNode :: rest)

let rec getSize node =
    match node with
    | FileNode (_, size) -> size  
    | DirectoryNode(_, nodes) -> 
        nodes 
        |> Seq.sumBy getSize 
    
let createNodes (commands : Command list) =
    let mutable rootNode = DirectoryNode("/", [])
    let mutable currentPath : string list = []
    for c in commands do
        match c with
        | Ls -> ()
        | GoToDirectory dirName -> 
            currentPath <- currentPath @ [dirName]
        | GoDirectoryUp -> 
            currentPath <- currentPath |> List.take (List.length currentPath - 1)
        | GoToRootDirectory -> 
            currentPath <- []
        | FoundSubDir(dirName) -> 
            rootNode <- add currentPath (DirectoryNode (dirName, []) ) rootNode
        | FoundFile(fileSize, fileName) -> 
            rootNode <- add currentPath (FileNode (fileName, fileSize) ) rootNode
    rootNode

let commands = run Parser.inputParser input |> unwrapParserResult
let rootNode = createNodes commands 

let q7a() =
    Console.WriteLine ""    
    rootNode
    |> traverse
    |> Seq.map (fun n -> (n, getSize n))
    |> Seq.collect (fun (n, size) -> 
            match n with 
            | DirectoryNode (dirName, _) -> [(dirName, size)]
            | _ -> []
            )
    |> Seq.filter (fun (name, size) -> size <= 100000)
    |> Seq.sumBy snd
    |> fun size -> Console.WriteLine $"7a: {size}"

let q7b() =
    Console.WriteLine ""
    let usedSpace = getSize rootNode
    let unused = 70_000_000 - usedSpace
    let toBeFreed = 30_000_000 - unused
    rootNode
    |> traverse
    |> Seq.map (fun n -> (n, getSize n))
    |> Seq.collect (fun (n, size) -> 
            match n with 
            | DirectoryNode (dirName, _) -> [(dirName, size)]
            | _ -> []
            )
    |> Seq.sortBy snd
    |> Seq.filter (fun (name, size) -> size >= toBeFreed)
    |> Seq.head
    |> fun (name, size) -> Console.WriteLine $"7b: {name} : {size}" 
    