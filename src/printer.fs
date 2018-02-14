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
        | Bool b -> Console.Write b
        | Nil -> Console.Write "nil"
        | Func f -> Console.Write "#<fun>"
        | l -> print_str l
        Console.Write  " "
        print_str (Lst(t))
    | Lst([]) ->
        Console.Write  " "
    | h ->
        match h with
        | String s -> Console.Write s
        | Number n -> Console.Write n
        | Symbol s -> Console.Write s
        | Bool b -> Console.Write b
        | Nil -> Console.Write "nil"
        | Func f -> Console.Write "#<fun>"
        | l -> print_str l
        Console.Write  " "