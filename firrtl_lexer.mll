(*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*)

{
open Firrtl_grammar
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

let whitespace = [ '\t' ' ']+

rule token = parse
  | comment { token lexbuf }
  | id as i { Id i }
  | relaxedid as r { RelaxedId r }
  | unsignedint as u { UnsignedInt u }
  | signedint as s { SignedInt s }
  | stringlit as s { StringLit s }
  | hexlit as h { HexLit h }
  | doublelit as d { DoubleLit d }
  | rawstring as r { RawString r }
