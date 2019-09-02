(*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*)

{
open Firrtl_grammar

let verbose = false
let lastind = ref 0

let (keywh:(string, token) Hashtbl.t) = Hashtbl.create 255
let (asidh:(token, string) Hashtbl.t) = Hashtbl.create 255
let keyword = Hashtbl.find keywh
let asid = Hashtbl.find asidh

let _ = List.iter 
      (fun (k,s) -> Hashtbl.add keywh s k; Hashtbl.add asidh k s)
      [
 CIRCUIT, "circuit";
 DEFNAME, "defname";
DATA_TYPE,"data-type";
READ_LATENCY, "read-latency";
WRITE_LATENCY, "write-latency";
READ_UNDER_WRITE,  "read-under-write";      
   Analog, "analog";
   ANALOG, "analog";
   ANDR, "andr";
   ASCLOCK, "asclock";
   ASFIXEDPOINT, "asfixedpoint";
   ATTACH, "attach";
   BPSET, "bpset";
   BPSHL, "bpshl";
   BPSHR, "bpshr";
   Clock, "Clock";
   CONNECTS, "connects";
   CMEM, "cmem";
   COLON, "colon";
   DEPTH, "depth";
   ELSE, "else";
   EQUALS, "equals";
   EXTMODULE, "extmodule";
   Fixed, "fixed";
   FIXED, "fixed";
   FLIP, "flip";
   HEAD, "head";
   INFER, "infer";
   INPUT, "input";
   INST, "inst";
   INVALID, "invalid";
   IS, "is";
   MEM, "mem";
   MODULE, "module";
   MPORT, "mport";
   NEG, "neg";
   NEW, "new";
   NODE, "node";
   OF, "of";
   OLD, "old";
   ORR, "orr";
   OUTPUT, "output";
   PAD, "pad";
   PARAMETER, "parameter";
   READ_LATENCY, "read_LATENCY";
   READ_UNDER_WRITE, "read_UNDER_WRITE";
   RDWR, "rdwr";
   READ, "read";
   READER, "reader";
   READWRITER, "readwriter";
   REG, "reg";
   RESET, "reset";
   SInt, "SInt";
   SKIP, "skip";
   SMEM, "smem";
   UInt, "UInt";
   UNDEFINED, "undefined";
   VALIDIF, "validif";
   WHEN, "when";
   WIRE, "wire";
   WITH, "with";
   WRITE, "write";
   WRITER, "writer";
   XORR, "xorr";
   ]

let tok arg = arg
}

let unsignedint = [ '0' - '9' ]+

let signedint = [ '+' '-' ] ['1'-'9'] [ '0' - '9']*

let posint = ['1'-'9'] [ '0' - '9']*

let hexlit = '"' 'h' [ '0'-'9' 'A'-'F' 'a'-'f' ] [ '0'-'9' 'A'-'F' 'a'-'f' ]* '"'

let doublelit = [ '+' '-' ]* ['0'-'9']+ '.' ['0'-'9']+ [ 'E' '+' '-' '0'-'9' ]

let unquotedstring = [^ '\r' '\n' ]*

let stringlit = '"' unquotedstring '"'

let rawstring = '\'' unquotedstring '\''

let legalstartchar = ['a'-'z' 'A'-'Z' '_']

let legalidchar = legalstartchar
  | ['0'-'9']
  | '$'
  | '-'

let id = legalstartchar (legalidchar)*

let relaxedid = (legalidchar)+

let comment = ';' [^ '\r' '\n']* '\n'

let info = '@' [^ '\r' '\n']*

let whitespace = [ '\t' ' ']+

let becomes1 = '<' '-'

let becomes2 = '<' '='

let connects = '=' '>'

let bits_ = 'b' 'i' 't' 's' '('

let lt_ = 'l' 't' '('

let eq_ = 'e' 'q' '('

let neq_ = 'n' 'e' 'q' '('

let leq_ = 'l' 'e' 'q' '('

let geq_ = 'g' 'e' 'q' '('

let gt_ = 'g' 't' '('

let and_ = 'a' 'n' 'd' '('

let or_ = 'o' 'r' '('

let xor_ = 'x' 'o' 'r' '('

let not_ = 'n' 'o' 't' '('

let printf_ = 'p' 'r' 'i' 'n' 't' 'f' '('

let stop_ = 's' 't' 'o' 'p' '('

let tail_ = 't' 'a' 'i' 'l' '('

let cat_ = 'c' 'a' 't' '('

let cvt_ = 'c' 'v' 't' '('

let mux_ = 'm' 'u' 'x' '('

let add_ = 'a' 'd' 'd' '('

let sub_ = 's' 'u' 'b' '('

let mul_ = 'm' 'u' 'l' '('

let div_ = 'd' 'i' 'v' '('

let rem_ = 'r' 'e' 'm' '('

let shr_ = 's' 'h' 'r' '('

let shl_ = 's' 'h' 'l' '('

let dshl_ = 'd' 's' 'h' 'l' '('

let dshr_ = 'd' 's' 'h' 'r' '('

let asuint_ = 'a' 's' 'U' 'I' 'n' 't' '('

let assint_ = 'a' 's' 'S' 'I' 'n' 't' '('

let validif_ = 'v' 'a' 'l' 'i' 'd' 'i' 'f' '('

rule token = parse
  | info { token lexbuf }
  | comment { token lexbuf }
  | whitespace { token lexbuf }
  | id as i { if verbose then print_endline i; try keyword i with Not_found -> Id i }
  | unsignedint as u { UnsignedInt (int_of_string u) }
  | signedint as s { SignedInt (int_of_string s) }
  | doublelit as d { DoubleLit d }
  | relaxedid as r { if verbose then print_endline r; RelaxedId r }
  | rawstring as r { if verbose then print_endline r; RawString r }
  | eof
      { tok ( EOF_TOKEN ) }
| becomes1
{ tok ( BECOMES1 ) }

| becomes2
{ tok ( BECOMES2 ) }

| connects
{ tok ( CONNECTS ) }

| asuint_
{ tok ( ASUINT ) }

| assint_
{ tok ( ASSINT ) }

| bits_
{ tok ( BITS ) }

| lt_
{ tok ( LT ) }

| eq_
{ tok ( EQ ) }

| neq_
{ tok ( NEQ ) }

| leq_
{ tok ( LEQ ) }

| geq_
{ tok ( GEQ ) }

| gt_
{ tok ( GT ) }

| and_
{ tok ( AND ) }

| or_
{ tok ( OR ) }

| xor_
{ tok ( XOR ) }

| not_
{ tok ( NOT ) }

| printf_
{ tok ( PRINTF ) }

| stop_
{ tok ( STOP ) }

| tail_
{ tok ( TAIL ) }

| cat_
{ tok ( CAT ) }

| cvt_
{ tok ( CVT ) }

| mux_
{ tok ( MUX ) }

| add_
{ tok ( ADD ) }

| sub_
{ tok ( SUB ) }

| mul_
{ tok ( MUL ) }

| div_
{ tok ( DIV ) }

| rem_
{ tok ( REM ) }

| shl_
{ tok ( SHL ) }

| shr_
{ tok ( SHR ) }

| dshl_
{ tok ( DSHL ) }

| dshr_
{ tok ( DSHR ) }

| validif_
{ tok ( VALIDIF ) }

| '!'
{ tok ( PLING ) }

| '"' as c
{ let s = String.make 1 c ^ dquote (Lexing.lexeme_start lexbuf) 1 lexbuf in if verbose then print_endline s;
 try Scanf.sscanf s "\"h%[0-9a-fA-F]" (fun h -> HexLit h) with _ -> StringLit s }

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
{ tok ( BRA ) }

| ')'
{ tok ( RPAREN ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RBRACE ) }

| '>'
{ tok ( KET ) }

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

| '\n' [ ' ' ]* as indent
{
let ind = String.length indent in
tok ( if ind < !lastind then (lastind := ind; UNINDENT) else if ind > !lastind then (lastind := ind; INDENT) else NEWLINE )
}

  | _ as c {
  let s = String.make 1 c in
  if verbose then print_endline s; RawString s }

and dquote start ix = parse
| '"' as c
    { String.make 1 c }
| '\\' '"' as s
    { String.sub s 1 1 ^ dquote start (ix+1) lexbuf }
| eof
    { failwith (Printf.sprintf "Unterminated \" string \" at offset %d." (start+ix)) }
| _ as c
    { String.make 1 c ^ dquote start (ix+1) lexbuf }

