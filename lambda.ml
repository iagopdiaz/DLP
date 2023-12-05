
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
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
;;

type command =
    Eval of term
  | Bind of string * term
  | Bindty of string * ty
;;

type vcontext =
  (string * term) list
;;
(* CONTEXT MANAGEMENT *)

let emptytctx =
  []
;;

let addtbinding tctx x bind =
  (x, bind) :: tctx
;;

let gettbinding tctx x =
  List.assoc x tctx
;;

let emptyvctx =
  []
;;

let addvbinding vctx x bind =
  (x, bind) :: vctx
;;

let getvbinding vctx x =
  List.assoc x vctx
;;
(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyTuple l ->
    let f ty = string_of_ty ty ^ ", " in
    let s = List.fold_left (^) "" (List.map f l) in
    let s' = if s <> "" then String.sub s 0 (String.length s - 2) else "" in
    "{" ^ s' ^ "}"
;;

exception Type_error of string
;;

let rec typeof tctx tm = match tm with      
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof tctx t1 = TyBool then
        let tyT2 = typeof tctx t2 in
        if typeof tctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof tctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof tctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding tctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let tctx' = addtbinding tctx x tyT1 in
      let tyT2 = typeof tctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tyT2 = typeof tctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof tctx t1 in
      let tctx' = addtbinding tctx x tyT1 in
      typeof tctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof tctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))

    (* New rule for string *)
  | TmString _ ->
      TyString

    (* New rule for string *)
  | TmConcat (t1, t2) ->
      if typeof tctx t1 = TyString && typeof tctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")
  
    (* New rule for string *)
  | TmLength t1 ->
      if typeof tctx t1 = TyString then TyNat
      else raise (Type_error "argument of length is not a string")
  
    (* T-Tuple *)
  | TmTuple fields -> 
      TyTuple (List.map (fun t -> typeof tctx t) fields)

    (* New rule for geting position *)
  | TmProj (t, s) -> 
      (match typeof tctx t with
         TyTuple fieldtype -> 
          (try List.nth fieldtype (int_of_string s - 1) with 
            _ -> raise (Type_error ("label " ^ s ^ " not found")))
        | _ -> raise(Type_error("tuple type expected"))) 

;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")" 
  | TmLength t ->
      "(length " ^ string_of_term t ^ ")"
  | TmTuple terms -> 
      "{" ^ String.concat ", " (List.map string_of_term terms) ^ "}"
  | TmProj (t, proj) ->
        string_of_term t ^ "." ^ proj
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLength t ->
      free_vars t
  | TmTuple fields -> 
      List.fold_left (fun freev typ -> lunion (free_vars typ) freev) [] fields
  | TmProj (t, _) ->
        free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString st ->
      TmString st
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmLength t ->
      TmLength (subst x s t)
  | TmTuple fields ->
      TmTuple (List.map (fun typ -> subst x s typ) fields)
  | TmProj (t, proj) ->
      TmProj (subst x s t, proj)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | TmTuple fields -> List.for_all (fun typ -> isval typ) fields
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t2), v2) when isval v2 ->
      subst x v2 t2

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'  

    (* E-Var *)
  | TmVar str -> (* now when we evaluate a variable you may get it from the vcontext*)
      getvbinding vctx str    

    (* New rule for string *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
     
    (* New rule for string *) 
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 vctx t2 in
      TmConcat (TmString s1, t2')

    (* New rule for string *)
  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat (t1', t2)  
    
  | TmLength (TmString s1) ->
    let length = String.length s1 in
    let rec term_of_int n = (* Función auxiliar para convertir un entero en término *)
      if n = 0 then
        TmZero
      else
        TmSucc (term_of_int (n - 1))
    in term_of_int length
  
  | TmLength t1 ->
      let t1' = eval1 vctx t1 in
      TmLength t1' 
  (* E-Tuple *)
  | TmTuple fields -> 
      let rec evalafield = function
        [] -> raise NoRuleApplies
        | vi::rest when isval vi -> 
          let rest' = evalafield rest in
            vi::rest'
        | ti::rest -> 
          let ti' = eval1 vctx ti in 
          ti'::rest
      in let fields' = evalafield fields in 
      TmTuple fields'
  
  (* E-Proj *)
  | TmProj (TmTuple fields as v1, lb) when isval v1 -> 
    (try List.nth fields (int_of_string lb - 1) with
     _ -> raise NoRuleApplies)

  | TmProj (t1, lb) -> 
    let t1' = eval1 vctx t1 in 
    TmProj (t1', lb)

  | _ ->
      raise NoRuleApplies

;;

(* Function for searching context*)
let search_context vctx tm = 
  let rec aux acum tm' = match tm' with
        TmTrue -> TmTrue
      | TmFalse -> TmFalse
      | TmZero -> TmZero
      | TmString s -> TmString s
      | TmIf (t1, t2, t3) -> TmIf (aux acum t1, aux acum t2, aux acum t3)
      | TmSucc t -> TmSucc (aux acum t)
      | TmPred t -> TmPred (aux acum t)
      | TmIsZero t -> TmIsZero (aux acum t)
      | TmVar str -> if (List.mem str acum) then TmVar str (* if the string is a known value return it *)
                     else getvbinding vctx str (* otherwise, try to find it in the vcontext *)
      | TmAbs (str, ty, t) -> TmAbs (str, ty, aux (str::acum) t)
      | TmApp (t1, t2) -> TmApp (aux acum t1, aux acum t2)
      | TmLetIn (str, t1, t2) -> TmLetIn (str, aux acum t1, aux (str::acum) t2) (* adding it to its own "context" *)
      | TmFix t -> TmFix (aux acum t)
      | TmConcat (t1, t2) -> TmConcat (aux acum t1, aux acum t2)
      | TmLength t -> TmLength (aux acum t)
      | TmTuple terms -> TmTuple (List.map (aux acum) terms)
      | TmProj (t, field) -> TmProj (aux acum t, field)
  in aux [] tm
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> search_context vctx tm
;;

let rec execute (vctx, tctx) comm = match comm with
    Eval tm -> let tyTm = typeof tctx tm in
                  let tm' = eval vctx tm in
                      print_endline (string_of_term tm' ^ " : " ^ string_of_ty tyTm);
                      (vctx, tctx)

  | Bind (str, tm) -> let tyTm = typeof tctx tm in
                        let tm' = eval vctx tm in
                          print_endline (str ^ " = " ^ string_of_term tm' ^ " : " ^ string_of_ty tyTm);
                          (addvbinding vctx str tm', addtbinding tctx str tyTm)

  | Bindty (str, tm) -> let ty' = tm in                       (*change for ty' = eval vctx tm when value admits types*)
                          print_endline (str ^ " : " ^ string_of_ty ty');
                          (vctx, addtbinding tctx str ty')               (*change for addvbinding vctx str tm' when value context admits types*)
;;
