
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
%token CONCAT
%token LENGTH
%token BOOL
%token NAT
%token STRING

%token LPAREN
%token RPAREN
%token LKEY
%token RKEY
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s : 
    IDV EQ ty EOF
      { Bindty ($1, $3)}
  | IDV EQ term EOF
      { Bind ($1, $3) }
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
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | pathTerm  
      { $1 }

    
pathTerm :
    pathTerm DOT INTV 
      { TmProj ($1, string_of_int $3) }
  | pathTerm DOT IDV
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
      { TmTuple $2}
  | LKEY record RKEY
      { TmRecord $2}

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

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LKEY tupleType RKEY
      { TyTuple $2}
  | LKEY recordType RKEY
      { TyRecord $2}

tupleType : 
    ty 
      {[$1]}
  | ty COMMA tupleType
      {$1 :: $3}

recordType :
    {[]}
  | STRINGV EQ ty
    {[($1, $3)]}
  | STRINGV EQ ty COMMA recordType
    {($1, $3) :: $5}