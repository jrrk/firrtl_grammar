(*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*)

{
open Firrtl_grammar

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
 CIRCUIT, "circuit";
 DEFNAME, "defname";
DATA_TYPE,"data_type";
READ_LATENCY, "read_latency";
WRITE_LATENCY, "write_latency";
READ_UNDER_WRITE,  "read_under_write";      
   Analog, "analog";
   ANALOG, "analog";
   ADD, "add";
   AND, "and";
   ANDR, "andr";
   ASCLOCK, "asclock";
   ASFIXEDPOINT, "asfixedpoint";
   ASSINT, "assint";
   ASUINT, "asuint";
   ATTACH, "attach";
   BECOMES1, "becomes1";
   BECOMES2, "becomes2";
   BITS, "bits";
   BPSET, "bpset";
   BPSHL, "bpshl";
   BPSHR, "bpshr";
   CAT, "cat";
   Clock, "Clock";
   CLOCK, "clock";
   CONNECTS, "connects";
   CMEM, "cmem";
   COLON, "colon";
   CVT, "cvt";
   DEPTH, "depth";
   DIV, "div";
   DSHL, "dshl";
   DSHR, "dshr";
   ELSE, "else";
   EQ, "eq";
   EQUALS, "equals";
   EXTMODULE, "extmodule";
   Fixed, "fixed";
   FIXED, "fixed";
   FLIP, "flip";
   GEQ, "geq";
   GREATER, "greater";
   GT, "gt";
   HEAD, "head";
   INFER, "infer";
   INPUT, "input";
   INST, "inst";
   INVALID, "invalid";
   IS, "is";
   LBRACK, "lbrack";
   LBRACE, "lbrace";
   LPAREN, "lparen";
   LEQ, "leq";
   LESS, "less";
   LT, "lt";
   MEM, "mem";
   MODULE, "module";
   MPORT, "mport";
   MUL, "mul";
   MUX, "mux";
   NEG, "neg";
   NEQ, "neq";
   NEW, "new";
   NODE, "node";
   NOT, "not";
   OF, "of";
   OLD, "old";
   OR, "or";
   ORR, "orr";
   OUTPUT, "output";
   PAD, "pad";
   PARAMETER, "parameter";
   DOT, "dot";
   PRINTF, "printf";
   RBRACK, "rbrack";
   RBRACE, "rbrace";
   READ_LATENCY, "read_LATENCY";
   READ_UNDER_WRITE, "read_UNDER_WRITE";
   RPAREN, "rparen";
   RDWR, "rdwr";
   READ, "read";
   READER, "reader";
   READWRITER, "readwriter";
   REG, "reg";
   REM, "rem";
   RESET, "reset";
   SHL, "shl";
   SHR, "shr";
   SInt, "sint";
   SKIP, "skip";
   SMEM, "smem";
   STOP, "stop";
   SUB, "sub";
   TAIL, "tail";
   UInt, "UInt";
   UNDEFINED, "undefined";
   VALIDIF, "validif";
   WHEN, "when";
   WIRE, "wire";
   WITH, "with";
   WRITE, "write";
   WRITER, "writer";
   XOR, "xor";
   XORR, "xorr";
      ];
    fun s -> Hashtbl.find h s


let tok arg = arg
(*
if !verbose then print_endline ( match arg with
  | ID id -> id
  | NUMBER n -> string_of_int n
  | CHAR ch -> String.make 1 ch
  | oth -> Ord.getstr oth );
  arg
*)
}

let unsignedint = [ '0' - '9' ]+

let signedint = [ '+' '-' ] ['1'-'9'] [ '0' - '9']*

let posint = ['1'-'9'] [ '0' - '9']*

let hexlit = '"' 'h' ( '+' | '-' )* ['0'-'9' 'A'-'F' 'a'-'f' ]+ '"'

let doublelit = [ '+' '-' ]* ['0'-'9']+ '.' ['0'-'9']+ [ 'E' '+' '-' '0'-'9' ]

let unquotedstring = '"'[^ '\r' '\n']*

let stringlit = '"' unquotedstring '"'

let rawstring = '\'' unquotedstring '\''

let legalstartchar = ['a'-'z' 'A'-'Z' '_']

let legalidchar = legalstartchar
  | ['0'-'9']
  | '$'

let id = legalstartchar (legalidchar)*

let relaxedid = (legalidchar)+

let comment = ';' [^ '\r' '\n']*

let info = '@' [^ '\r' '\n']*

let whitespace = [ '\t' ' ']+

let becomes1 = '<' '-'

let becomes2 = '<' '='

rule token = parse
  | info { token lexbuf }
  | comment { token lexbuf }
  | whitespace { token lexbuf }
  | id as i { print_endline i; try keyword i with Not_found -> Id i }
  | unsignedint as u { UnsignedInt u }
  | signedint as s { SignedInt s }
  | stringlit as s { StringLit s }
  | hexlit as h { HexLit h }
  | doublelit as d { DoubleLit d }
  | relaxedid as r { print_endline r; RelaxedId r }
  | rawstring as r { print_endline r; RawString r }
  | eof
      { tok ( EOF_TOKEN ) }
| becomes1
{ tok ( BECOMES1 ) }

| becomes2
{ tok ( BECOMES2 ) }

| '!'
{ tok ( PLING ) }

| '"'
{ tok ( DOUBLEQUOTE ) }

| '#'
{ tok ( HASH ) }

| '$'
{ tok ( DOLLAR ) }

| '%'
{ tok ( PERCENT ) }

| '&'
{ tok ( AMPERSAND ) }

| '''
{ tok ( QUOTE ) }

| '('
{ tok ( LPAREN ) }

| '['
{ tok ( LBRACK ) }

| '{'
{ tok ( LBRACE ) }

| '<'
{ tok ( LESS ) }
| ')'
{ tok ( RPAREN ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RBRACE ) }

| '>'
{ tok ( GREATER ) }

| '*'
{ tok ( STAR ) }

| '+'
{ tok ( PLUS ) }

| ','
{ tok ( token lexbuf ) }

| '-'
{ tok ( HYPHEN ) }

| '.'
{ tok ( DOT ) }

| '/'
{ tok ( SLASH ) }

| '\\'
{ tok ( BACKSLASH ) }

| ':'
{ tok ( COLON ) }

| ';'
{ tok ( SEMICOLON ) }

| '='
{ tok ( EQUALS ) }

| '?'
{ tok ( QUERY ) }

| '@'
{ tok ( AT ) }

| '^'
{ tok ( CARET ) }

| '_'
{ tok ( UNDERSCORE ) }

| '`'
{ tok ( BACKQUOTE ) }

| '|'
{ tok ( VBAR ) }

| '~'
{ tok ( TILDE ) }

| '\012'
{ token lexbuf }

  | _ as c {
  if int_of_char c = 10 then token lexbuf else (
  let s = String.make 1 c in
  print_endline s; RawString s) }
