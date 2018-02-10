module Make.A.Lisp.Types

open System

type Type =
    | Nil
    | Ture
    | False
    | String of string
    | Number of int
    | Symbol of string
    | Bool of bool
    | Undefined of string
    | Lst of Type list
    | Func of (Type list -> int)

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