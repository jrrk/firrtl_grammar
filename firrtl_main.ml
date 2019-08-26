open Firrtl
open Firrtl_lexer

let parse_from_chan ch =
  lex_init();
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output
					    
let parse arg =
  let ch = open_in arg in
  try
  print_endline ("**** Parsing "^arg^" ****");
  let log = open_out (arg^".err") in
  Unix.dup2 (Unix.descr_of_out_channel log) Unix.stderr;
  print_string ("stderr redirected to "^arg^".err - ");
  let rslt = parse_from_chan ch in
  close_in ch;
  print_endline "completed.";
  rslt
  with  e ->
    print_endline ("** Error ** "^Printexc.to_string e);
    Printexc.print_backtrace stderr;
    close_in ch;
  EMPTY_TOKEN
