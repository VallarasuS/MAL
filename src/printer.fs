module Make.A.Lisp.Printer

open System
open Make.A.Lisp.Types

let rec print_str str =
    match str with
    | h::t -> 
        match h with
        | String s -> Console.Write s
        | Number n -> Console.Write n
        | Symbol s -> Console.Write s
        | List l -> print_str l
        | _ -> printfn ""
        Console.Write  " "
        print_str t
    | [] -> printfn ""