open Firrtl_grammar
open Firrtl_lexer
open Firrtl_dump

let lastind = ref 0
let lastidx = ref 0
let back = ref None
let indent_debug = ref false

let rec indent str len ix = match str.[ix] with
  | ' ' -> if len > ix+1 then indent str len (ix+1) else len
  | _ -> ix

let rec from_func ch indent_log dst wid =
  let lin = match !back with
  | None -> (try input_line ch^"\n" with End_of_file -> "")
  | Some lin -> lin in
  let len = String.length lin in
  let ind = if len > 0 then indent lin len 0 else 0 in
  if len = ind+1 then
    begin
    from_func ch indent_log dst wid
    end
  else
    begin
    let (lin',len',ind') = if ind < !lastind - 2 then
      begin
      lastind := !lastind - 2;
      let indstr = String.make !lastind ' ' ^ "\n" in
      let len' = String.length indstr in
      back := Some lin;
      String.blit indstr 0 dst 0 len';
      indstr,len',!lastind
      end
    else
      begin
      if len <= wid + !lastidx then
	  begin
	  lastind := ind;
	  back := None;
	  let len' = len - !lastidx in
	  let lin' = String.sub lin !lastidx len' in
	  String.blit lin !lastidx dst 0 len';
	  lastidx := 0;
	  lin',len',ind
	  end
      else
	  begin
	  let lin' = String.sub lin !lastidx wid in
	  back := Some lin;
	  String.blit lin !lastidx dst 0 wid;
	  lastidx := !lastidx + wid;
	  lin',wid,ind
	  end
      end in
    if !indent_debug then
	begin
	Printf.fprintf indent_log "%4d,%4d:%s####\n" ind' !lastidx (String.sub lin' 0 (len'-1));
	flush indent_log;
	end
    else
	output_string indent_log (String.sub dst 0 len');
    len'
    end

let parse_from_chan ch indent_log =
  let lb = Lexing.from_function (from_func ch indent_log) in
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
  let indent_log = open_out "indent.log" in
  let rslt = parse_from_chan ch indent_log in
  close_out indent_log;
  close_in ch;
  print_endline "completed.";
  rslt
  with  e ->
    print_endline ("** Error ** "^Printexc.to_string e);
    Printexc.print_backtrace stderr;
    close_in ch;
  TNone

let modhash = Hashtbl.create 255

let modules modnam = function
| TUPLE5 (MODULE, Id nam, COLON, TLIST portlst, TLIST stmtlst) ->
  print_endline nam;
  Hashtbl.add modhash nam (portlst, stmtlst);
  if (modnam = "") || (modnam=nam) then dump nam portlst stmtlst;
| TUPLE6 (EXTMODULE, Id nam, COLON, TLIST portlst, defname, TLIST stmtlst) -> ()
| oth -> failwith "modules"

let iterate fil modnam =
  match parse fil with
    | TUPLE4 (CIRCUIT, Id top, COLON, TLIST modlst) -> List.iter (modules modnam) modlst
    | oth -> failwith "circuit"
