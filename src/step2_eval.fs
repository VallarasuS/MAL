module Make.A.Lisp.step2

    open Make.A.Lisp.Reader
    open Make.A.Lisp.Printer
    open Make.A.Lisp.Types

    open System

    let READ input =
        read_str input

    let rec EVAL env ast  =
        match ast with
        | Lst([]) -> ast
        | Lst(ht) as l -> 
            match eval_ast env l with
            | Lst(Func(f)::t) -> Number( f t )
            | ht -> ht
        | t -> eval_ast env t

    and eval_ast (env:Env) ast =
        match ast with
        | Lst(Symbol(h)::t)-> 
            match Env.get env h with
            | Some v -> Lst(v::t) 
            | None -> failwith "not found"
        | Lst(h) -> Lst( h |> List.map (fun a -> EVAL env a))

        | _ -> ast

    let PRINT exp =
        print_str exp

    let rep str =
        str
        |> READ
        |> EVAL Env.defaultEnv
        |> PRINT

    [<EntryPoint>]
    let rec main argv =

        Console.WriteLine();
        Console.Write "user>"
        Console.Out.Flush();
        let input = Console.ReadLine()
    
        match input with
        | "" -> 0
        | inp -> 
            rep inp
            main argv