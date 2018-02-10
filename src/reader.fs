module Make.A.Lisp.Reader

    open Tokenizer
    open Types

    let position = 0
    
    let rec read_form ht =
        let rec read l acc =
            match l with
            | OpenParentheses ::t -> 
                match read_list t with
                | Some (h), t -> read t (h::acc)
                | None, t 
                    -> Some(acc) , t
            | t -> 
                match read_atom t with
                | Some(h), t -> read t (h::acc)
                | None, t 
                    -> Some(acc |> List.rev) , t

        read ht []
    
    and read_atom ht =
        match ht with
        | Token.Token("nil") :: t -> Some(Type.Nil),t
        | Token.Token("true") :: t -> Some(Type.Bool(true)),t
        | Token.Token("false") :: t -> Some(Type.Bool(false)),t
        | Token.String s ::t -> Some(Types.String(s)),t
        | Token.Number n::t -> Some(Type.Number(n |> int)), t 
        | Token.Token s :: t -> Some(Type.Symbol(s)),t
        | t -> None, t

    and read_list ht =
        let rec read l acc =
            match l with
            | [] -> failwith "expected )"
            | CloseParentheses :: t -> Some (Lst(acc)), t
            | t ->
                match read_form t with
                | Some(h), t -> read t (h @ acc)
                | None, t -> read t acc
        
        read ht []
    
    let rec readForms acc ht =
        match ht with 
        | [] -> List.head acc
        | t ->
            match read_form t with
            | Some(h), t -> readForms (h @ acc) t 
            | None, t -> readForms acc t

    let read_str str =
        str
        |> tokenize
        |> readForms []