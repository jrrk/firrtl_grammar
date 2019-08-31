open Firrtl_grammar
open Firrtl_lexer

let parse_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      circuit token lb
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
  let rslt = parse_from_chan ch in
  close_in ch;
  print_endline "completed.";
  rslt
  with  e ->
    print_endline ("** Error ** "^Printexc.to_string e);
    Printexc.print_backtrace stderr;
    close_in ch;
  TNone

let modules modnam = function
| TUPLE5 (MODULE, Id nam, COLON, TLIST portlst, TLIST stmtlst) ->
  print_endline nam;
  if (modnam = "") || (modnam=nam) then Trial.trial nam portlst stmtlst;
| TUPLE6 (EXTMODULE, Id nam, COLON, TLIST portlst, defname, TLIST stmtlst) -> ()
| oth -> failwith "modules"

let iterate fil modnam =
  match parse fil with
    | TUPLE4 (CIRCUIT, Id top, COLON, TLIST modlst) -> List.iter (modules modnam) modlst
    | oth -> failwith "circuit"
