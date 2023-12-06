type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | FIX
  | IN
  | CONCAT
  | LENGTH
  | BOOL
  | NAT
  | STRING
  | LIST
  | LPAREN
  | RPAREN
  | LKEY
  | RKEY
  | DOT
  | COMMA
  | EQ
  | COLON
  | ARROW
  | EOF
  | RSQUARE
  | LSQUARE
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | INTV of (
# 45 "parser.mly"
        int
# 42 "parser.mli"
)
  | IDV of (
# 46 "parser.mly"
        string
# 47 "parser.mli"
)
  | STRINGV of (
# 47 "parser.mly"
        string
# 52 "parser.mli"
)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
