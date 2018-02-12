module Make.A.Lisp.step4

    open System

    open Make.A.Lisp.Reader
    open Make.A.Lisp.Printer
    open Make.A.Lisp.Types
    open Make.A.Lisp.Env
    open System.Diagnostics
    open System.Xml

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
        | Lst(Symbol("do") :: ht ) -> doForm env (Lst ht)
        | Lst(Symbol("if") :: ht ) -> ifForm env (Lst ht)
        | Lst(Symbol("fun*") :: ht ) -> funStar env (Lst ht)
        | Lst([]) -> ast
        | Lst(ht) as l -> evalFun env l
        | t -> eval_ast env t

    and funStar outer ast =
        let makefn binds body =
            let fn = fun nodes -> 
                let inner = Env.makeNew outer binds nodes
                EVAL inner body
            Fun(fn)
            
        match ast with
        | Lst([binds;body]) -> makefn [binds] body
        | _  -> failwith "un expected arity"

    and ifForm env ast = 
        match ast with
        |Lst([condform; trueform; falseform]) -> ifForm3 env condform trueform falseform
        |Lst([condform; trueform]) -> ifForm3 env condform trueform Nil
        | _ -> failwith "unexpected form"

    and ifForm3 env condform trueform falseform  =
        match EVAL env condform with
        | Bool(false) | Nil -> EVAL env falseform
        | _ -> EVAL env trueform

    and doForm env ast =
        match ast with 
        | Lst(h::t) -> 
            EVAL env h|> ignore
            doForm env (Lst t)
        | t -> EVAL env t

    and evalFun env ast =
         match eval_ast env ast with
            | Lst(Func(f)::t) -> t |> List.map (fun a -> EVAL env a) |> f |> Number
            | ht -> ht

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