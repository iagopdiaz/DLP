
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty 
;;


type tcontext =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmLength of term
  | TmTuple of term list
  | TmProj of term * string
  | TmRecord of (string * term) list
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term 
  | TmHead of ty * term 
  | TmTail of ty * term 
;;

type vcontext =
  (string * term) list
;;

type command =
    Eval of term
  | Bind of string * term
  | Bindty of string * ty
;;

val emptytctx : tcontext;;
val addtbinding : tcontext -> string -> ty -> tcontext;;
val gettbinding : tcontext -> string -> ty;;

val emptyvctx : vcontext;;
val addvbinding : vcontext -> string -> term -> vcontext;;
val getvbinding : vcontext -> string -> term;;


val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : tcontext -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : vcontext -> term -> term;;

val execute : vcontext * tcontext -> command -> vcontext * tcontext;;
