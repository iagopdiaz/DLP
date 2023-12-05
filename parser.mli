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
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
