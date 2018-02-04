module Make.A.Lisp.step1

open Make.A.Lisp.Reader
open Make.A.Lisp.Printer

open System

let READ input =
    read_str input

let EVAL evn ast  =
    ast

let PRINT exp =
    print_str exp

let rep str =
    str
    |> READ
    |> EVAL ""
    |> PRINT

[<EntryPoint>]
let rec main argv =

    Console.Write "user>"
    Console.Out.Flush();
    let input = Console.ReadLine()
    
    match input with
    | "" -> 0
    | inp -> 
        rep inp
        main argv