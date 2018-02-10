#load "types.fs"
#load "tokenizer.fs"
#load "reader.fs"
#load "printer.fs"
#load "env.fs"
#load "step4_if_fn_do.fs"

open Make.A.Lisp.Reader
open Make.A.Lisp.Printer
open Make.A.Lisp.Types
open Make.A.Lisp.Env

open Make.A.Lisp.step4

let test arg =
    rep arg

test "(+ 1 2)"
test "(+ 1 (+ 1 1))"

test "(def! b 6)"

test "(let* (c 2) c)"

test "(let* ((c 2)(d 3)) d)"
