module Make.A.Lisp.Env

    open Types
    open Make.A.Lisp.Core

    let makeEnv () = Env();

    let oflist lst =
        let env = Env()
        let accumulate (e:Env) (k, v) = e.Add(k,v); e
        List.fold accumulate env lst

    let defaultEnv = 
        [
            "+", Core.add
            "-", Core.subtract
            "*", Core.multiply
            "/", Core.divide
        ]
        |> List.map (fun (k, v) -> k, Func(v))
        |> oflist

    let rec find (chain:EnvChain) k =
        match chain with
        | [] -> None
        | env :: t -> 
            match env.TryGetValue(k) with
            | true, v -> Some v
            | false, _ -> find t k

    let get chain k = find chain k

    let set (env:Env) k v =
        env.Add(k,v);

    let makeNew outer symbols nodes =
      let envc =  makeEnv() :: outer
      let env = List.head envc

      let rec loop symbols nodes =
        match symbols, nodes with
        | Lst([Symbol("&"); Symbol(s)]), nodes ->
            set env s nodes
            envc
        | Lst(Symbol(s):: symbols), Lst (n::nodes) ->
            set env s n 
            loop (Lst symbols) (Lst nodes)
        | Lst [], Lst [] -> envc
        | _ -> failwith "unexpected"

      loop symbols nodes 





