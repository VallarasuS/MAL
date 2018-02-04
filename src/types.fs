module Make.A.Lisp.Types

type Type =
    | Nil
    | Ture
    | False
    | String of string
    | Number of string
    | Symbol of string
    | Undefined of string
    | List of Type list

    