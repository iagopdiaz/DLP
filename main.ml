open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let rec read_command acc =
  let line = read_line () in
  if String.ends_with ~suffix:";;" line then
    let last_line = String.sub line 0 (String.length line - 2) in 
    let full_acc = last_line :: acc in
    String.concat " " (List.rev full_acc)
  else
    read_command (line :: acc)

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let input_text = read_command [] in
      let tm = s token (from_string input_text) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
      | Lexical_error ->
          print_endline "lexical error";
          loop ctx
      | Parse_error ->
          print_endline "syntax error";
          loop ctx
      | Type_error e ->
          print_endline ("type error: " ^ e);
          loop ctx
      | End_of_file ->
          print_endline "...bye!!!"
  in
  loop emptyctx
;;

top_level_loop ()

