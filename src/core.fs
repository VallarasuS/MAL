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