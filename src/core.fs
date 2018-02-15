module Make.A.Lisp.Core

    open Make.A.Lisp.Types

    let rec apply f ast =
        match ast with
        | Lst([Number(a); Number(b)]) -> f (a |> int) (b |> int) |> Number
        | _ -> failwith "Invalid Operation"


    let add = apply( fun a b -> a + b)

    let subtract = apply ( fun a b -> a - b)

    let multiply = apply (fun a b -> a * b)

    let divide = apply(fun a b -> a / b)

    let list = Lst

    let isList = function
        | Lst [_] ->  Type.True
        | Lst [] -> Type.False
        | _ -> Type.False

    let isEmpty = function
        | Lst (h::t) -> 
            isList h
        | _ -> Type.False

    let count = function
        | Lst(h::t) -> 
            match h with 
            | Lst(i) -> List.length i
            | _ -> 0
        | _ -> 0

    let equal = failwith "TODO"
        