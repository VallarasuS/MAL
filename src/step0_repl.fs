module Make.A.Lisp.step0

open System

let READ input =
    input

let EVAL evn ast  =
    ast

let PRINT exp =
    printfn "%s" exp

let rep str =
    str
    |> READ
    |> EVAL ""
    |> PRINT

//[<EntryPoint>]
let rec main argv =

    Console.Write "user>"
    Console.Out.Flush();
    let input = Console.ReadLine()
    
    match input with
    | "" -> 0
    | inp -> 
        rep inp
        main argv