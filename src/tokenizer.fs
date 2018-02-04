module Make.A.Lisp.Tokenizer

    open System

    type Token =
        | Eof
        | OpenParentheses | CloseParentheses
        | OpenBracket | CloseBracket
        | OpenBrace | CloseBrace
        | SingleQuote
        | BackTick
        | Tilde
        | SpliceUnquote
        | Caret
        | At
        | String of string
        | Token of string
        | Keyword of string
        | Number of string

    let tokenize (str:string) =
        
        let p = 0;
        let len = str.Length
        
        let inline isDigit c = Char.IsDigit(c)
        let inline isWhiteSpace c = c = ',' || Char.IsWhiteSpace c
        let inline isTokenChar c = 
            match c with
            | '('  | ')' 
            | '['  | ']' 
            | '{'  | '}' 
            | '\'' | '`' | ','
            | '~'  | '^'  | '@'  | '"'  | ':'  -> false
            | ch when Char.IsWhiteSpace ch -> false
            | _ -> true;

        let accumulateSpliceUnquote p =
            if (p >= len) then
                Tilde, p
            elif (str.[p] = '@') then
                SpliceUnquote, p + 1
            else
                Tilde, p

        let accumulateString p =
            let rec accChar p escape acc =
                if (p >= len) then failwith "unexpected EOF"
                let c = str.[p]
                let n = p + 1
                match c with
                | x when escape -> accChar n false ('\\'::x::acc) // TODO : test whether it works fine
                | '\\' -> accChar n true acc
                | '"' -> (List.rev acc), n
                | _ -> accChar n false (c::acc)

            let acc, n = accChar p false []
            let r = string(acc |> List.toArray)
            String r, n
            
        let rec accumulateWhile pred f start p =
            if (p >= len) then
                str.Substring(start, p - start) |> f, p
            elif (pred str.[p]) then 
                (p + 1) |> accumulateWhile pred f start
            else
                str.Substring(start, p - start) |> f, p

        let accumulateKeyword p =
            let n = p + 1
            if (p >= len) then 
                failwith "unexpected EOF"
            elif isTokenChar str.[p] then 
                accumulateWhile isTokenChar Keyword p n
            else 
                failwith "unexpected char"

        let rec gettoken p = 
        
            if (p >= len) then
                Eof, p
            else
                let n = p + 1
                match str.[p] with
                | c when isWhiteSpace c -> gettoken n
                | '(' -> OpenParentheses, n
                | ')' -> CloseParentheses, n
                | '[' -> OpenBracket, n
                | ']' -> CloseBracket, n
                | '{' -> OpenBrace, n
                | '}' -> CloseBrace, n
                | '\'' -> SingleQuote, n
                | '`' -> BackTick, n
                | '~' -> accumulateSpliceUnquote n
                | '^' -> Caret, n
                | '@' -> At, n
                | '"' -> accumulateString n
                | ':' -> accumulateKeyword n
                | '-' when isDigit str.[n] -> accumulateWhile isDigit Number p n
                | c when isDigit c -> accumulateWhile isDigit Number p n
                | c when isTokenChar c -> accumulateWhile isDigit Token p n
                | _ -> failwith "unexpected"
            
        let rec accumulate acc p =
             match gettoken p with
             | Eof, p -> List.rev acc
             | t, p -> accumulate (t::acc) p

        accumulate [] 0