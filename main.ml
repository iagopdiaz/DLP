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
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let input_text = read_command [] in
      let typecomm = s token (from_string input_text) in
      loop (execute (vctx, tctx) typecomm)
    with
      | Lexical_error ->
          print_endline "lexical error";
          loop (vctx, tctx)
      | Parse_error ->
          print_endline "syntax error";
          loop (vctx, tctx)
      | Type_error e ->
          print_endline ("type error: " ^ e);
          loop (vctx, tctx)
      | End_of_file ->
          print_endline "...bye!!!"
  in
  loop (emptyvctx, emptytctx)
;;

top_level_loop ()

