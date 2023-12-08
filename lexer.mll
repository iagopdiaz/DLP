
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "fix"       { FIX }
  | "in"        { IN }
  | "as"        { AS }
  | "case"      { CASE }
  | "of"        { OF }
  | "concat"    { CONCAT }
  | "length"    { LENGTH }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { STRING }
  | "list"      { LIST }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LKEY }
  | '}'         { RKEY }
  | '<'         { LTHAN }
  | '>'         { GTHAN }
  | '.'         { DOT }
  | ','         { COMMA }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "nil"       { NIL }
  | "cons"      { CONS }
  | "["         { LSQUARE }
  | "]"         { RSQUARE }
  | "isnil"     { ISNIL }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['A'-'Z']['a'-'z' '_' '0'-'9']*
                 { IDT (Lexing.lexeme lexbuf) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { IDV (Lexing.lexeme lexbuf) }
  | '"'[^ '"' ';' '\n']*'"'
                { let s = Lexing.lexeme lexbuf in
                  STRINGV (String.sub s 1 (String.length s - 2)) }
  | eof         { EOF }
  | _           { raise Lexical_error }

