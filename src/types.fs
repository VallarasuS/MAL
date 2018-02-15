module Make.A.Lisp.Types

open System

type Type =
    | Nil
    | True
    | False
    | String of string
    | Number of int
    | Symbol of string
    | Undefined of string
    | Lst of Type list
    | Vector of Type array
    | Func of (Type -> Type)

 type Env = System.Collections.Generic.Dictionary<string,Type>

 type EnvChain = Env list

 // helpers

 let inline (|Pair|_|) t =
    match t with
    | Lst(Lst([f;s])::t) -> Some (Lst([f;s]), Lst(t))
    | Lst([f;s]) -> Some (Lst([f;s]), Lst([]))
    | Lst([]) -> None
    | Lst(_) -> failwith "invalid no of args"
    | _ -> None