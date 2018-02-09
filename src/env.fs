﻿module Make.A.Lisp.Env

    open Types

    let makeEnv () = Env();

    let oflist lst =
        let env = Env()
        let accumulate (e:Env) (k, v) = e.Add(k,v); e
        List.fold accumulate env lst

    let apply f ast =
        match ast with
        | [Number(a); Number(b)] -> f (a |> int) (b |> int)
        | _ -> failwith "Invalid Operation"

    let defaultEnv = 
        [
        "+", apply (fun a b -> a + b);
        "-", apply (fun a b -> a - b);
        "*", apply (fun a b -> a * b);
        "/", apply (fun a b -> a / b);
        ]
        |> List.map ( fun (k, v) -> k, Func(v))
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

    let makeNew outer s n =
        makeEnv() :: outer


