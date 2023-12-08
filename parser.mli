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
  | CASE
  | AS
  | OF
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
  | LTHAN
  | GTHAN
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
  | INTV of (int)
  | IDT of (string)
  | IDV of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
