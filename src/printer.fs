module Make.A.Lisp.Printer

open System
open Make.A.Lisp.Types

let rec print_str ast =
    match ast with
    | Lst(h::t) -> 
        match h with
        | String s -> Console.Write s
        | Number n -> Console.Write n
        | Symbol s -> Console.Write s
        | l -> print_str l
        Console.Write  " "
        print_str (Lst(t))
    | h -> 
        match h with
        | String s -> Console.Write s
        | Number n -> Console.Write n
        | Symbol s -> Console.Write s
        | l -> print_str l
        Console.Write  " "
