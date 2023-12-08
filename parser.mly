
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token FIX
%token IN
%token CASE
%token AS
%token OF
%token CONCAT
%token LENGTH
%token BOOL
%token NAT
%token STRING
%token LIST

%token LPAREN
%token RPAREN
%token LKEY
%token RKEY
%token LTHAN
%token GTHAN
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token EOF

%token RSQUARE
%token LSQUARE
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL

%token <int> INTV
%token <string> IDT
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s : 
    IDT EQ ty EOF
      { Bindty ($1, $3)}
  | IDV EQ term EOF
      { Bind ($1, $3) }
  | ty EOF
      { EvalTy $1 }
  | term EOF
      { Eval $1 }
  
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8)}
      
appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | LENGTH atomicTerm
      { TmLength $2}
  | NIL LSQUARE ty RSQUARE
      { TmNil $3}
  | CONS LSQUARE ty RSQUARE atomicTerm atomicTerm
      { TmCons ($3, $5, $6) }
  | ISNIL LSQUARE ty RSQUARE atomicTerm
      { TmIsNil ($3, $5) }
  | HEAD LSQUARE ty RSQUARE atomicTerm
      { TmHead ($3, $5) }
  | TAIL LSQUARE ty RSQUARE atomicTerm
      { TmTail ($3, $5) }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | projection  
      { $1 }

projection :
    projection DOT INTV 
      { TmProj ($1, string_of_int $3) }
  | projection DOT IDV
      { TmProj ($1, $3) }
  | atomicTerm
      { $1 }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | LKEY tuple RKEY
      { TmTuple $2 }
  | LKEY record RKEY
      { TmRecord $2 }
  | LTHAN IDV EQ term GTHAN AS ty
      { TmTagging ($2, $4, $7)}
  
tuple : 
  term 
      {[$1]}
  | term COMMA tuple
      {$1 :: $3}

record :
    {[]}
  | IDV EQ term
    {[($1, $3)]}
  | IDV EQ term COMMA record
    {($1, $3) :: $5}

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | atomicTy LIST
      { TyList $1 }

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | IDT
      { TyVar $1}
  | LKEY tupleType RKEY
      { TyTuple $2 }
  | LKEY recordType RKEY
      { TyRecord $2 }
  | LTHAN variantType GTHAN
      { TyVariant $2 }

tupleType : 
    ty 
      {[$1]}
  | ty COMMA tupleType
      {$1 :: $3}

recordType :
    {[]}
  | IDV COLON ty
    {[($1, $3)]}
  | IDV COLON ty COMMA recordType
    {($1, $3) :: $5}

variantType : 
    IDV COLON ty
    {[($1, $3)]}
  | IDV COLON ty COMMA variantType
    {($1, $3) :: $5}