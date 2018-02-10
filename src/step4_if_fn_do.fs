module Make.A.Lisp.step4

    open System

    open Make.A.Lisp.Reader
    open Make.A.Lisp.Printer
    open Make.A.Lisp.Types
    open Make.A.Lisp.Env

    let READ input =
        read_str input

    let rec iterPair f bindings =
        match bindings with
        |  Pair(h,t) -> 
            f h |> ignore
            iterPair f t
        | _ -> []

    let rec EVAL env ast  =
        match ast with
        | Lst(Symbol("def!") :: ht) -> defEnv env (Lst ht)
        | Lst(Symbol("let*") :: ht ) -> letStar env (Lst ht)
        | Lst([]) -> ast
        | Lst(ht) as l -> 
            match eval_ast env l with
            | Lst(Func(f)::t) -> t |> List.map (fun a -> EVAL env a) |> f |> Number
            | ht -> ht
        | t -> eval_ast env t

    and defEnv env ast =
        match ast with
        | Lst([Symbol(k); v])  -> 
            let res = EVAL env v
            let h = List.head env
            Env.set h k res
            res

        | _ -> failwith "Invalid args"
    
    and letStar env ast =
        match ast with
        | Lst([bindings;form]) -> 
            let newEnv = makeNew env [] []
            let binder = defEnv newEnv 
            match bindings with
            | Lst([_;_]) -> iterPair binder bindings |> ignore
            | _ -> failwith "Wrong arity?"
            EVAL newEnv form
        | _ -> failwith "unexpected"

    and eval_ast (env:EnvChain) ast =
        match ast with
        | Lst(Symbol(h)::t)-> 
            match Env.get env h with
            | Some v -> Lst(v::t) 
            | None -> failwith "not found"
        | Symbol(h) -> 
            match Env.get env h with
            | Some v -> v
            | None -> failwith "not found"
        | Lst(h) -> Lst( h |> List.map (fun a -> EVAL env a))

        | _ -> ast

    let PRINT exp =
        print_str exp

    let rep str =
        str
        |> READ
        |> EVAL [Env.defaultEnv]
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