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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 39 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* FIX *);
  269 (* IN *);
  270 (* CONCAT *);
  271 (* LENGTH *);
  272 (* BOOL *);
  273 (* NAT *);
  274 (* STRING *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* LKEY *);
  278 (* RKEY *);
  279 (* DOT *);
  280 (* COMMA *);
  281 (* EQ *);
  282 (* COLON *);
  283 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTV *);
  285 (* IDV *);
  286 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\006\000\006\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\007\000\007\000\002\000\002\000\008\000\008\000\008\000\
\008\000\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\004\000\004\000\002\000\001\000\006\000\006\000\006\000\008\000\
\001\000\002\000\002\000\002\000\003\000\002\000\002\000\001\000\
\003\000\001\000\003\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\003\000\001\000\003\000\003\000\001\000\001\000\
\001\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\020\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\024\000\037\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\010\000\011\000\012\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\003\000\015\000\000\000\
\000\000\000\000\000\000\000\000\013\000\019\000\000\000\025\000\
\031\000\032\000\033\000\000\000\000\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\000\000\001\000\002\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\034\000\029\000\006\000\005\000\
\007\000\000\000\036\000\000\000\008\000"

let yydgoto = "\002\000\
\019\000\066\000\035\000\021\000\022\000\023\000\036\000\056\000\
\067\000"

let yysindex = "\004\000\
\027\255\000\000\229\254\000\000\000\000\110\255\050\255\050\255\
\050\255\233\254\244\254\050\255\050\255\110\255\110\255\000\000\
\253\254\000\000\000\000\026\000\050\255\000\000\017\255\021\255\
\000\000\044\255\000\000\000\000\000\000\025\255\032\255\050\255\
\000\000\039\255\036\255\040\255\080\255\000\000\000\000\033\255\
\249\254\110\255\110\255\249\254\000\000\000\000\110\255\000\000\
\000\000\000\000\000\000\080\255\080\255\063\000\064\000\038\255\
\000\000\249\254\249\254\043\255\061\255\055\255\045\255\000\000\
\052\255\053\255\063\255\000\000\000\000\249\254\110\255\110\255\
\110\255\110\255\000\000\249\254\000\000\000\000\000\000\000\000\
\000\000\079\255\000\000\110\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\025\000\000\000\000\000\000\000\051\000\001\000\013\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\071\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\078\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\219\255\002\000\000\000\011\000\000\000\055\000\000\000\
\028\000"

let yytablesize = 331
let yytable = "\054\000\
\009\000\024\000\020\000\060\000\001\000\030\000\063\000\026\000\
\049\000\050\000\051\000\058\000\016\000\059\000\065\000\034\000\
\031\000\027\000\028\000\029\000\065\000\037\000\032\000\033\000\
\022\000\038\000\028\000\003\000\004\000\005\000\006\000\039\000\
\078\000\007\000\008\000\009\000\010\000\011\000\055\000\040\000\
\012\000\013\000\045\000\061\000\062\000\014\000\041\000\015\000\
\042\000\043\000\004\000\004\000\005\000\034\000\016\000\017\000\
\018\000\044\000\046\000\047\000\057\000\048\000\068\000\069\000\
\070\000\071\000\072\000\073\000\014\000\074\000\015\000\075\000\
\079\000\080\000\081\000\082\000\076\000\016\000\025\000\018\000\
\003\000\004\000\005\000\006\000\077\000\085\000\007\000\008\000\
\009\000\010\000\011\000\084\000\026\000\012\000\013\000\049\000\
\050\000\051\000\052\000\035\000\053\000\064\000\000\000\083\000\
\000\000\000\000\000\000\016\000\025\000\018\000\003\000\004\000\
\005\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000\
\011\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\014\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\025\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\009\000\000\000\009\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\009\000\016\000\016\000\
\000\000\016\000\016\000\009\000\009\000\009\000\009\000\018\000\
\009\000\016\000\022\000\022\000\009\000\009\000\009\000\016\000\
\016\000\016\000\016\000\000\000\016\000\000\000\000\000\000\000\
\016\000\016\000\016\000\022\000\000\000\022\000\028\000\022\000\
\028\000\028\000\028\000\028\000\022\000\022\000\022\000\004\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\
\004\000\000\000\004\000"

let yycheck = "\037\000\
\000\000\029\001\001\000\041\000\001\000\029\001\044\000\006\000\
\016\001\017\001\018\001\019\001\000\000\021\001\052\000\014\000\
\029\001\007\000\008\000\009\000\058\000\025\001\012\000\013\000\
\000\000\000\000\000\000\001\001\002\001\003\001\004\001\021\000\
\070\000\007\001\008\001\009\001\010\001\011\001\037\000\023\001\
\014\001\015\001\032\000\042\000\043\000\019\001\026\001\021\001\
\005\001\025\001\000\000\002\001\003\001\052\000\028\001\029\001\
\030\001\026\001\020\001\024\001\028\001\022\001\000\000\000\000\
\027\001\023\001\006\001\013\001\019\001\025\001\021\001\020\001\
\071\000\072\000\073\000\074\000\024\001\028\001\029\001\030\001\
\001\001\002\001\003\001\004\001\022\001\084\000\007\001\008\001\
\009\001\010\001\011\001\013\001\022\001\014\001\015\001\016\001\
\017\001\018\001\019\001\022\001\021\001\047\000\255\255\076\000\
\255\255\255\255\255\255\028\001\029\001\030\001\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\014\001\015\001\255\255\255\255\255\255\
\019\001\255\255\021\001\255\255\255\255\255\255\255\255\255\255\
\255\255\028\001\029\001\030\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\002\001\003\001\
\255\255\005\001\006\001\019\001\020\001\021\001\022\001\023\001\
\024\001\013\001\002\001\003\001\028\001\029\001\030\001\019\001\
\020\001\021\001\022\001\255\255\024\001\255\255\255\255\255\255\
\028\001\029\001\030\001\019\001\255\255\021\001\020\001\023\001\
\022\001\023\001\024\001\025\001\028\001\029\001\030\001\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\255\255\255\255\255\255\255\255\255\255\020\001\255\255\
\022\001\255\255\024\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  FIX\000\
  IN\000\
  CONCAT\000\
  LENGTH\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LPAREN\000\
  RPAREN\000\
  LKEY\000\
  RKEY\000\
  DOT\000\
  COMMA\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 47 "parser.mly"
      ( Bindty (_1, _3))
# 272 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 49 "parser.mly"
      ( Bind (_1, _3) )
# 280 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 51 "parser.mly"
      ( Eval _1 )
# 287 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 55 "parser.mly"
      ( _1 )
# 294 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 57 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 303 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 312 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 321 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8))
# 331 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( _1 )
# 338 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      ( TmSucc _2 )
# 345 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmPred _2 )
# 352 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIsZero _2 )
# 359 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmConcat (_2, _3) )
# 367 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmLength _2)
# 374 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      ( TmApp (_1, _2) )
# 382 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( _1 )
# 389 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
      ( TmProj (_1, string_of_int _3) )
# 397 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( _1 )
# 404 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 92 "parser.mly"
      ( _2 )
# 411 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
      ( TmTrue )
# 417 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
      ( TmFalse )
# 423 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 98 "parser.mly"
      ( TmVar _1 )
# 430 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 100 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 440 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "parser.mly"
      ( TmString _1 )
# 447 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple) in
    Obj.repr(
# 107 "parser.mly"
      ( TmTuple _2)
# 454 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 111 "parser.mly"
      ([_1])
# 461 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 113 "parser.mly"
      (_1 :: _3)
# 469 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 117 "parser.mly"
      ( _1 )
# 476 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 119 "parser.mly"
      ( TyArr (_1, _3) )
# 484 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 123 "parser.mly"
      ( _2 )
# 491 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
      ( TyBool )
# 497 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
      ( TyNat )
# 503 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
      ( TyString )
# 509 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleType) in
    Obj.repr(
# 131 "parser.mly"
      ( TyTuple _2)
# 516 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 135 "parser.mly"
      ([_1])
# 523 "parser.ml"
               : 'tupleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleType) in
    Obj.repr(
# 137 "parser.mly"
      (_1 :: _3)
# 531 "parser.ml"
               : 'tupleType))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
