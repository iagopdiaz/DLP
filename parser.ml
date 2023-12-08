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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 53 "parser.ml"
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
  270 (* CASE *);
  271 (* AS *);
  272 (* OF *);
  273 (* CONCAT *);
  274 (* LENGTH *);
  275 (* BOOL *);
  276 (* NAT *);
  277 (* STRING *);
  278 (* LIST *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* LKEY *);
  282 (* RKEY *);
  283 (* LTHAN *);
  284 (* GTHAN *);
  285 (* DOT *);
  286 (* COMMA *);
  287 (* EQ *);
  288 (* COLON *);
  289 (* ARROW *);
    0 (* EOF *);
  290 (* RSQUARE *);
  291 (* LSQUARE *);
  292 (* NIL *);
  293 (* CONS *);
  294 (* ISNIL *);
  295 (* HEAD *);
  296 (* TAIL *);
    0|]

let yytransl_block = [|
  297 (* INTV *);
  298 (* IDT *);
  299 (* IDV *);
  300 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\006\000\006\000\
\006\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\007\000\007\000\008\000\008\000\008\000\002\000\
\002\000\002\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\010\000\010\000\011\000\011\000\011\000\012\000\
\012\000\000\000"

let yylen = "\002\000\
\004\000\004\000\002\000\002\000\001\000\006\000\006\000\006\000\
\008\000\001\000\002\000\002\000\002\000\003\000\002\000\004\000\
\006\000\005\000\005\000\005\000\002\000\001\000\003\000\003\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\007\000\001\000\003\000\000\000\003\000\005\000\001\000\
\003\000\002\000\003\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\001\000\003\000\000\000\003\000\005\000\003\000\
\005\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\027\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\044\000\045\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\000\000\031\000\058\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\011\000\012\000\013\000\000\000\000\000\000\000\015\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\000\004\000\021\000\000\000\042\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\043\000\026\000\000\000\000\000\000\000\000\000\032\000\033\000\
\048\000\049\000\000\000\000\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\024\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\052\000\036\000\000\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\001\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\019\000\
\020\000\007\000\006\000\008\000\000\000\000\000\039\000\055\000\
\000\000\057\000\017\000\000\000\034\000\009\000"

let yydgoto = "\002\000\
\029\000\053\000\054\000\032\000\033\000\034\000\055\000\056\000\
\035\000\057\000\058\000\060\000"

let yysindex = "\007\000\
\139\255\000\000\215\254\000\000\000\000\021\255\094\255\094\255\
\094\255\223\254\228\254\094\255\094\255\000\000\000\000\000\000\
\183\255\125\000\232\254\242\254\247\254\254\254\006\255\007\255\
\000\000\014\255\016\255\000\000\000\000\049\000\050\000\094\255\
\000\000\022\255\234\254\023\255\021\255\228\255\010\255\000\000\
\049\255\000\000\000\000\000\000\025\255\034\255\094\255\000\000\
\000\000\043\255\044\255\231\254\041\255\042\255\047\255\048\255\
\050\255\051\255\004\255\054\255\163\000\163\000\163\000\163\000\
\163\000\163\000\021\255\000\000\000\000\000\000\218\254\000\000\
\163\000\163\000\052\255\053\255\021\255\021\255\163\000\000\000\
\000\000\000\000\021\255\163\000\163\000\021\255\000\000\000\000\
\000\000\000\000\021\255\163\000\000\000\163\000\097\000\032\255\
\045\255\059\255\064\255\067\255\068\255\078\000\085\000\000\000\
\000\000\000\000\074\255\098\255\093\255\076\255\078\255\079\255\
\000\000\000\000\083\255\082\255\081\255\084\255\000\000\094\255\
\094\255\094\255\094\255\000\000\000\000\021\255\021\255\021\255\
\021\255\071\255\072\255\107\255\032\255\094\255\000\000\000\000\
\000\000\000\000\000\000\000\000\110\255\052\255\000\000\000\000\
\163\000\000\000\000\000\021\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\108\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\069\000\000\000\000\000\000\000\000\000\095\000\
\034\000\063\000\001\000\000\000\000\000\108\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\151\000\118\255\119\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\151\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\126\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\127\255\128\255\
\000\000\000\000\000\000\096\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\108\255\126\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\026\000\003\000\000\000\005\000\000\000\047\000\006\000\
\000\000\070\000\030\000\032\000"

let yytablesize = 461
let yytable = "\072\000\
\040\000\036\000\104\000\031\000\105\000\083\000\084\000\001\000\
\041\000\045\000\073\000\042\000\043\000\044\000\046\000\047\000\
\047\000\048\000\059\000\051\000\061\000\003\000\004\000\005\000\
\006\000\062\000\030\000\007\000\008\000\009\000\010\000\011\000\
\063\000\010\000\091\000\092\000\070\000\012\000\013\000\051\000\
\064\000\065\000\050\000\037\000\066\000\038\000\067\000\039\000\
\068\000\069\000\071\000\080\000\076\000\077\000\074\000\078\000\
\020\000\021\000\022\000\023\000\024\000\025\000\022\000\040\000\
\028\000\079\000\081\000\082\000\029\000\103\000\085\000\086\000\
\087\000\088\000\118\000\089\000\090\000\124\000\119\000\108\000\
\109\000\093\000\083\000\091\000\125\000\111\000\097\000\098\000\
\099\000\100\000\101\000\102\000\120\000\115\000\005\000\004\000\
\005\000\121\000\106\000\107\000\122\000\123\000\126\000\127\000\
\110\000\128\000\129\000\130\000\131\000\112\000\132\000\133\000\
\084\000\142\000\117\000\092\000\037\000\116\000\038\000\050\000\
\039\000\145\000\148\000\056\000\134\000\135\000\136\000\137\000\
\138\000\139\000\140\000\141\000\114\000\037\000\025\000\143\000\
\040\000\028\000\147\000\003\000\004\000\005\000\006\000\051\000\
\035\000\007\000\008\000\009\000\010\000\011\000\150\000\053\000\
\038\000\054\000\113\000\012\000\013\000\014\000\015\000\016\000\
\144\000\017\000\000\000\018\000\146\000\019\000\000\000\000\000\
\000\000\000\000\149\000\000\000\000\000\000\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\003\000\
\004\000\005\000\006\000\000\000\000\000\007\000\008\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\012\000\
\013\000\014\000\015\000\016\000\000\000\017\000\000\000\018\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\021\000\022\000\023\000\024\000\025\000\
\049\000\040\000\028\000\000\000\003\000\004\000\005\000\006\000\
\000\000\000\000\007\000\008\000\009\000\010\000\011\000\000\000\
\000\000\000\000\000\000\000\000\012\000\013\000\000\000\000\000\
\000\000\000\000\037\000\000\000\038\000\000\000\039\000\000\000\
\000\000\000\000\040\000\040\000\000\000\040\000\040\000\020\000\
\021\000\022\000\023\000\024\000\025\000\040\000\075\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\000\000\000\000\040\000\010\000\010\000\047\000\010\000\010\000\
\000\000\040\000\000\000\040\000\040\000\000\000\010\000\000\000\
\047\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\010\000\010\000\010\000\010\000\010\000\025\000\010\000\
\022\000\022\000\000\000\022\000\022\000\000\000\029\000\029\000\
\000\000\000\000\010\000\022\000\010\000\010\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\022\000\022\000\
\022\000\022\000\022\000\029\000\022\000\029\000\000\000\029\000\
\000\000\029\000\000\000\005\000\005\000\000\000\000\000\022\000\
\000\000\022\000\022\000\005\000\000\000\029\000\000\000\029\000\
\029\000\000\000\000\000\014\000\015\000\016\000\005\000\094\000\
\005\000\095\000\005\000\096\000\005\000\003\000\004\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\000\000\049\000\117\000\000\000\012\000\013\000\014\000\
\015\000\016\000\000\000\017\000\000\000\018\000\000\000\019\000\
\029\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\020\000\021\000\022\000\023\000\024\000\025\000\049\000\052\000\
\028\000\000\000\000\000\000\000\000\000\029\000\000\000\029\000\
\029\000\029\000\000\000\029\000\029\000\014\000\015\000\016\000\
\000\000\094\000\000\000\095\000\000\000\096\000\000\000\029\000\
\000\000\029\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\049\000"

let yycheck = "\022\001\
\000\000\043\001\041\001\001\000\043\001\031\001\032\001\001\000\
\006\000\043\001\033\001\007\000\008\000\009\000\043\001\000\000\
\012\000\013\000\043\001\017\000\035\001\001\001\002\001\003\001\
\004\001\035\001\001\000\007\001\008\001\009\001\010\001\011\001\
\035\001\000\000\031\001\032\001\032\000\017\001\018\001\037\000\
\035\001\035\001\017\000\023\001\031\001\025\001\031\001\027\001\
\000\000\000\000\029\001\047\000\043\001\005\001\032\001\031\001\
\036\001\037\001\038\001\039\001\040\001\041\001\000\000\043\001\
\044\001\032\001\024\001\024\001\000\000\067\000\030\001\030\001\
\026\001\026\001\043\001\026\001\026\001\000\000\034\001\077\000\
\078\000\028\001\031\001\031\001\000\000\083\000\061\000\062\000\
\063\000\064\000\065\000\066\000\034\001\091\000\000\000\002\001\
\003\001\034\001\073\000\074\000\034\001\034\001\029\001\006\001\
\079\000\013\001\031\001\030\001\030\001\084\000\028\001\030\001\
\032\001\043\001\043\001\032\001\023\001\092\000\025\001\094\000\
\027\001\015\001\013\001\028\001\120\000\121\000\122\000\123\000\
\126\000\127\000\128\000\129\000\086\000\026\001\041\001\130\000\
\043\001\044\001\134\000\001\001\002\001\003\001\004\001\026\001\
\026\001\007\001\008\001\009\001\010\001\011\001\148\000\026\001\
\026\001\026\001\085\000\017\001\018\001\019\001\020\001\021\001\
\131\000\023\001\255\255\025\001\133\000\027\001\255\255\255\255\
\255\255\255\255\145\000\255\255\255\255\255\255\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\001\001\
\002\001\003\001\004\001\255\255\255\255\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\255\255\023\001\255\255\025\001\
\255\255\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\255\255\001\001\002\001\003\001\004\001\
\255\255\255\255\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\255\255\255\255\
\255\255\255\255\023\001\255\255\025\001\255\255\027\001\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\036\001\
\037\001\038\001\039\001\040\001\041\001\013\001\043\001\044\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\255\255\255\255\034\001\002\001\003\001\022\001\005\001\006\001\
\255\255\041\001\255\255\043\001\044\001\255\255\013\001\255\255\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\002\001\003\001\255\255\005\001\006\001\255\255\002\001\003\001\
\255\255\255\255\041\001\013\001\043\001\044\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\026\001\027\001\028\001\023\001\030\001\025\001\255\255\027\001\
\255\255\029\001\255\255\005\001\006\001\255\255\255\255\041\001\
\255\255\043\001\044\001\013\001\255\255\041\001\255\255\043\001\
\044\001\255\255\255\255\019\001\020\001\021\001\024\001\023\001\
\026\001\025\001\028\001\027\001\030\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\042\001\043\001\255\255\017\001\018\001\019\001\
\020\001\021\001\255\255\023\001\255\255\025\001\255\255\027\001\
\002\001\003\001\255\255\255\255\255\255\255\255\255\255\255\255\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\044\001\255\255\255\255\255\255\255\255\023\001\255\255\025\001\
\026\001\027\001\255\255\029\001\030\001\019\001\020\001\021\001\
\255\255\023\001\255\255\025\001\255\255\027\001\255\255\041\001\
\255\255\043\001\044\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\042\001"

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
  CASE\000\
  AS\000\
  OF\000\
  CONCAT\000\
  LENGTH\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  LIST\000\
  LPAREN\000\
  RPAREN\000\
  LKEY\000\
  RKEY\000\
  LTHAN\000\
  GTHAN\000\
  DOT\000\
  COMMA\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  RSQUARE\000\
  LSQUARE\000\
  NIL\000\
  CONS\000\
  ISNIL\000\
  HEAD\000\
  TAIL\000\
  "

let yynames_block = "\
  INTV\000\
  IDT\000\
  IDV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 62 "parser.mly"
      ( Bindty (_1, _3))
# 376 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 64 "parser.mly"
      ( Bind (_1, _3) )
# 384 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 66 "parser.mly"
      ( EvalTy _1 )
# 391 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 68 "parser.mly"
      ( Eval _1 )
# 398 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( _1 )
# 405 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 74 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 414 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 76 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 423 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 78 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 432 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
      ( TmLetIn (_2, TmFix (TmAbs (_2, _4, _6)), _8))
# 442 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 84 "parser.mly"
      ( _1 )
# 449 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 86 "parser.mly"
      ( TmSucc _2 )
# 456 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( TmPred _2 )
# 463 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 90 "parser.mly"
      ( TmIsZero _2 )
# 470 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 92 "parser.mly"
      ( TmConcat (_2, _3) )
# 478 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 94 "parser.mly"
      ( TmLength _2)
# 485 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 96 "parser.mly"
      ( TmNil _3)
# 492 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 98 "parser.mly"
      ( TmCons (_3, _5, _6) )
# 501 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 100 "parser.mly"
      ( TmIsNil (_3, _5) )
# 509 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 102 "parser.mly"
      ( TmHead (_3, _5) )
# 517 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 104 "parser.mly"
      ( TmTail (_3, _5) )
# 525 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 106 "parser.mly"
      ( TmApp (_1, _2) )
# 533 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projection) in
    Obj.repr(
# 108 "parser.mly"
      ( _1 )
# 540 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'projection) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
      ( TmProj (_1, string_of_int _3) )
# 548 "parser.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'projection) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "parser.mly"
      ( TmProj (_1, _3) )
# 556 "parser.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 116 "parser.mly"
      ( _1 )
# 563 "parser.ml"
               : 'projection))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 120 "parser.mly"
      ( _2 )
# 570 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
      ( TmTrue )
# 576 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
      ( TmFalse )
# 582 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
      ( TmVar _1 )
# 589 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 128 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 599 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
      ( TmString _1 )
# 606 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple) in
    Obj.repr(
# 135 "parser.mly"
      ( TmTuple _2 )
# 613 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'record) in
    Obj.repr(
# 137 "parser.mly"
      ( TmRecord _2 )
# 620 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 139 "parser.mly"
      ( TmTagging (_2, _4, _7))
# 629 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 143 "parser.mly"
      ([_1])
# 636 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 145 "parser.mly"
      (_1 :: _3)
# 644 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    Obj.repr(
# 148 "parser.mly"
    ([])
# 650 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 150 "parser.mly"
    ([(_1, _3)])
# 658 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'record) in
    Obj.repr(
# 152 "parser.mly"
    ((_1, _3) :: _5)
# 667 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 156 "parser.mly"
      ( _1 )
# 674 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 158 "parser.mly"
      ( TyArr (_1, _3) )
# 682 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTy) in
    Obj.repr(
# 160 "parser.mly"
      ( TyList _1 )
# 689 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 164 "parser.mly"
      ( _2 )
# 696 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 166 "parser.mly"
      ( TyBool )
# 702 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parser.mly"
      ( TyNat )
# 708 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 170 "parser.mly"
      ( TyString )
# 714 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 172 "parser.mly"
      ( TyVar _1)
# 721 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleType) in
    Obj.repr(
# 174 "parser.mly"
      ( TyTuple _2 )
# 728 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordType) in
    Obj.repr(
# 176 "parser.mly"
      ( TyRecord _2 )
# 735 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'variantType) in
    Obj.repr(
# 178 "parser.mly"
      ( TyVariant _2 )
# 742 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 182 "parser.mly"
      ([_1])
# 749 "parser.ml"
               : 'tupleType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleType) in
    Obj.repr(
# 184 "parser.mly"
      (_1 :: _3)
# 757 "parser.ml"
               : 'tupleType))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "parser.mly"
    ([])
# 763 "parser.ml"
               : 'recordType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 189 "parser.mly"
    ([(_1, _3)])
# 771 "parser.ml"
               : 'recordType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'recordType) in
    Obj.repr(
# 191 "parser.mly"
    ((_1, _3) :: _5)
# 780 "parser.ml"
               : 'recordType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 195 "parser.mly"
    ([(_1, _3)])
# 788 "parser.ml"
               : 'variantType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'variantType) in
    Obj.repr(
# 197 "parser.mly"
    ((_1, _3) :: _5)
# 797 "parser.ml"
               : 'variantType))
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
