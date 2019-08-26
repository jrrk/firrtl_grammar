/*
Copyright (c) 2012-2014, The Regents of the University of California
(Regents).  All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of the Regents nor the
   names of its contributors may be used to endorse or promote products
   derived from this software without specific prior written permission.

IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
*/

%token <string> Id
%token <string> RelaxedId
%token <string> UnsignedInt
%token <string> SignedInt
%token <string> StringLit
%token <string> HexLit
%token <string> DoubleLit
%token <string> RawString
%token MODULE
%token EXTMODULE
%token CIRCUIT
%token DEFNAME PARAMETER
%token INPUT OUTPUT FLIP WIRE REG MEM CMEM SMEM MPORT INST NODE STOP PRINTF SKIP ATTACH IS INVALID OF WITH WHEN ELSE MUX VALIDIF
%token INFER READ WRITE RDWR OLD NEW UNDEFINED CLOCK ANALOG FIXED 
%token RESET DATA_TYPE DEPTH READ_LATENCY  WRITE_LATENCY READ_UNDER_WRITE READER WRITER READWRITER
%token UInt SInt Fixed Clock Analog
%token LCURLY RCURLY LBRACK RBRACK COLON PERIOD LESS GREATER EQUALS LPAREN RPAREN BECOMES1 BECOMES2 CONNECTS
  %token ANALOG
  %token ADD
  %token AND
  %token ANDR
  %token ASCLOCK
  %token ASFIXEDPOINT
  %token ASSINT
  %token ASUINT
  %token ATTACH
  %token BITS
  %token BPSET
  %token BPSHL
  %token BPSHR
  %token CAT
  %token Clock
  %token CLOCK
  %token CMEM
  %token COLON
  %token CVT
  %token DEPTH
  %token DIV
  %token DSHL
  %token DSHR
  %token ELSE
  %token EQ
  %token EXTMODULE
  %token Fixed
  %token FIXED
  %token FLIP
  %token GEQ
  %token GT
  %token HEAD
  %token HexLit
  %token INFER
  %token INPUT
  %token INST
  %token INVALID
  %token IS
  %token LCURLY
  %token LEQ
  %token LT
  %token MEM
  %token MODULE
  %token MPORT
  %token MUL
  %token MUX
  %token NEG
  %token NEQ
  %token NEW
  %token NODE
  %token NOT
  %token OF
  %token OLD
  %token OR
  %token ORR
  %token OUTPUT
  %token PAD
  %token PARAMETER
  %token PRINTF
  %token RDWR
  %token READ
  %token READER
  %token READWRITER
  %token REG
  %token RelaxedId
  %token REM
  %token RESET
  %token SHL
  %token SHR
  %token SignedInt
  %token SInt
  %token SKIP
  %token SMEM
  %token STOP
  %token SUB
  %token TAIL
  %token UInt
  %token UNDEFINED
  %token UnsignedInt
  %token VALIDIF
  %token WHEN
  %token WIRE
  %token WITH
  %token WRITE
  %token WRITER
  %token XOR
  %token XORR
%token TNone
%token <token array> TARRAY
%token <token list> TLIST
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%type <token list> simple_stmt_lst id_lst suite
%type <token> circuit simple_stmt stmt reset_block suite_opt
%start circuit
%%

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

circuit
  : CIRCUIT id COLON   module_lst { TUPLE4(CIRCUIT,$2,COLON,TLIST $4) }
  ;

module_lst
  : { [] }
  | module_lst module1 { $2 :: $1 }
  
module1
  : MODULE id COLON   port_lst moduleBlock { TUPLE5(MODULE,$2,COLON,TLIST $4,$5) }
  | EXTMODULE id COLON   port_lst defname_opt parameter_lst { TUPLE6(EXTMODULE,$2,COLON,TLIST $4,$5,TLIST $6) }
  ;

defname_opt
  : { TNone }
  | defname { $1 }
  
port_lst
  : { [] }
  | port_lst port { $2 :: $1 }
  
parameter_lst
  : { [] }
  | parameter_lst parameter { $2 :: $1 }
  
port
  : dir id COLON type1  { TUPLE4($1,$2,COLON,$4)}
  ;

dir
  : INPUT { INPUT }
  | OUTPUT { OUTPUT }
  ;

type1 
  : UInt intlit_opt { TUPLE2(UInt, $2) }
  | SInt intlit_opt { TUPLE2(SInt, $2) }
  | Fixed intlit_opt intlit2_opt { TUPLE3(Fixed,$2,$3) }
  | Clock { Clock }
  | Analog intlit_opt { TUPLE2(Analog,$2) }
  | LCURLY field_lst RCURLY { TUPLE3(LCURLY,TLIST $2,RCURLY) }       // Bundle
  | type1 LBRACK intLit RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }   // Vector
  ;

field_lst
  : { [] }
  | field_lst field { $2 :: $1 }
  
intlit_opt
  : LESS intLit GREATER { TUPLE3(LESS,$2,GREATER) }
  
intlit2_opt
  : LESS LESS intLit GREATER GREATER { TUPLE5(LESS, LESS, $3, GREATER, GREATER) }
  
field
  : flip_opt fieldId COLON type1 { TUPLE4($1,$2,COLON,$4) }
  ;
  
flip_opt
  : { TNone }
  | FLIP { FLIP }
  
defname
  : DEFNAME EQUALS id { TUPLE3(DEFNAME,EQUALS,$3) }
  ;

parameter
  : PARAMETER id EQUALS intLit { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  | PARAMETER id EQUALS stringLit { TUPLE4(PARAMETER,$2,EQUALS, $4) }
  | PARAMETER id EQUALS doubleLit { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  | PARAMETER id EQUALS rawString { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  ;

moduleBlock
  : simple_stmt_lst { TLIST $1 }
  ;

simple_stmt_lst
  : { [] }
  | simple_stmt_lst simple_stmt { $2 :: $1 }

simple_reset0:  RESET CONNECTS LPAREN exp exp RPAREN { TUPLE6(RESET, CONNECTS, LPAREN, $4, $5, RPAREN) }

simple_reset
	: simple_reset0 { $1 }
	| LPAREN simple_reset0 RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
	;

reset_block
	: simple_reset { $1 }
	| LPAREN simple_reset RPAREN { TUPLE3(LPAREN,$2,RPAREN) }
  ;

stmt
  : WIRE id COLON type1 { TUPLE4(WIRE,$2,COLON,$4) }
  | REG id COLON type1 exp with_opt { TUPLE6(REG,$2,COLON,$4,$5,$6) }
  | MEM id COLON memField_lst { TUPLE4(MEM,$2,COLON,TLIST $4) }
  | CMEM id COLON type1  { TUPLE4(CMEM,$2,COLON,$4) }
  | SMEM id COLON type1  { TUPLE4(SMEM,$2,COLON,$4) }
  | mdir MPORT id EQUALS id LBRACK exp RBRACK exp  { TUPLE9($1,MPORT,$3,EQUALS,$5,LBRACK,$7,RBRACK,$9) }
  | INST id OF id  { TUPLE4(INST,$2,OF,$4) }
  | NODE id EQUALS exp { TUPLE4(NODE,$2,EQUALS,$4) }
  | exp BECOMES2 exp  { TUPLE3(BECOMES2,$1,$3) }
  | exp BECOMES1 exp  { TUPLE3(BECOMES1,$1,$3) }
  | exp IS INVALID {TUPLE3($1,IS,INVALID) }
  | when1 { $1 }
  | STOP exp exp intLit RPAREN  { TUPLE5(STOP,$2,$3,$4,RPAREN) }
  | PRINTF exp exp stringLit exp_lst RPAREN  { TUPLE6(PRINTF,$2,$3,$4,TLIST $5,RPAREN) }
  | SKIP  { SKIP}
  | ATTACH LPAREN exp_lst RPAREN { TUPLE4(ATTACH,LPAREN,TLIST $3,RPAREN) }
  ;

exp_lst
  : { [] }
  | exp_lst exp { $2 :: $1 }

memField_lst
  : { [] }
  | memField_lst memField { $2 :: $1 }

with_opt: { TNone }
  | WITH COLON reset_block { TUPLE3(WITH,COLON,$3) }
  
memField
	: DATA_TYPE CONNECTS type1 { TUPLE3(DATA_TYPE,CONNECTS,$3) }
	| DEPTH CONNECTS intLit { TUPLE3(DEPTH,CONNECTS,$3) }
	| READ_LATENCY CONNECTS intLit { TUPLE3(READ_LATENCY,CONNECTS,$3) }
	| WRITE_LATENCY CONNECTS intLit { TUPLE3(WRITE_LATENCY,CONNECTS,$3) }
	| READ_UNDER_WRITE CONNECTS ruw { TUPLE3(READ_UNDER_WRITE,CONNECTS,$3) }
	| READER CONNECTS id_lst { TUPLE3(READER,CONNECTS,TLIST $3) }
	| WRITER CONNECTS id_lst { TUPLE3(WRITER,CONNECTS,TLIST $3) }
	| READWRITER CONNECTS id_lst { TUPLE3(READWRITER,CONNECTS,TLIST $3) }
	;

id_lst
  : id { [ $1 ] }
  | id_lst id { $2 :: $1 }
  
simple_stmt
  : stmt { $1 }
  | { TNone }
  ;

/*
    We should provide syntatctical distinction between a "moduleBody" and a "suite":
    - statements require a "suite" which means they can EITHER have a "simple statement" (one-liner) on the same line
        OR a group of one or more _indented_ statements after a new-line. A "suite" may _not_ be empty
    - modules on the other hand require a group of one or more statements without any indentation to follow "port"
        definitions. Let's call that _the_ "moduleBody". A "moduleBody" could possibly be empty
*/
suite
  : simple_stmt { [ $1 ] }
  | suite simple_stmt { $2 :: $1 }
  ;

when1
  : WHEN exp COLON suite_opt else_when_opt { TUPLE5(WHEN, $2, COLON, $4, $5) }
  ;

suite_opt
  : { TNone }
  | suite { TLIST $1 }
  
when_opt
  : when1 { $1 }
  | COLON suite_opt { TUPLE2(COLON, $2) }

else_when_opt
  : { TNone }
  | ELSE when_opt { TUPLE2(ELSE,$2) }
  
info
  : { TNone }
  ;

mdir
  : INFER { INFER }
  | READ { READ }
  | WRITE { WRITE }
  | RDWR { RDWR }
  ;

ruw
  : OLD { OLD }
  | NEW { NEW }
  | UNDEFINED { UNDEFINED }
  ;

exp
  : UInt intlit_opt LPAREN intLit RPAREN { TUPLE5(UInt, $2, LPAREN, $4, RPAREN) }
  | SInt intlit_opt LPAREN intLit RPAREN { TUPLE5(SInt, $2, LPAREN, $4, RPAREN) }
  | id  { $1 }   // Ref
  | exp PERIOD fieldId { TUPLE3($1,PERIOD,$3) }
  | exp PERIOD doubleLit  { TUPLE3($1,PERIOD,$3) } // TODO Workaround for #470
  | exp LBRACK intLit RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }
  | exp LBRACK exp RBRACK { TUPLE4($1,LBRACK,$3,RBRACK) }
  | MUX exp exp exp RPAREN { TUPLE5(MUX,$2,$3,$4,RPAREN) }
  | VALIDIF exp exp RPAREN { TUPLE4(VALIDIF,$2,$3,RPAREN) }
  | primop exp_lst intLit_lst RPAREN { TUPLE3($1,TLIST $2,TLIST $3) }
  ;

intLit_lst
  : { [] }
  | intLit_lst intLit { $2 :: $1 }
  
id
  : Id { Id $1 }
  | keywordAsId { $1 }
  ;

fieldId
  : Id { Id $1 }
  | RelaxedId { RelaxedId $1 }
  | UnsignedInt { UnsignedInt $1 }
  | keywordAsId { $1 }
  ;

intLit
  : UnsignedInt { UnsignedInt $1 }
  | SignedInt { SignedInt $1 }
  | HexLit { HexLit $1 }
  ;

stringLit
  : StringLit { StringLit $1 }
  
rawString
  : RawString { RawString $1 }
  
doubleLit
  : DoubleLit { DoubleLit $1 }
  
// Keywords that are also legal ids
keywordAsId
  : CIRCUIT { CIRCUIT }
  | MODULE { MODULE}
  | EXTMODULE { EXTMODULE }
  | PARAMETER { PARAMETER }
  | INPUT { INPUT }
  | OUTPUT { OUTPUT }
  | UInt { UInt }
  | SInt { SInt }
  | CLOCK { CLOCK }
  | ANALOG { ANALOG }
  | FIXED { FIXED }
  | FLIP { FLIP }
  | WIRE { WIRE }
  | REG { REG }
  | WITH { WITH }
  | RESET { RESET }
  | MEM { MEM }
  | DEPTH { DEPTH }
  | READER { READER }
  | WRITER { WRITER }
  | READWRITER { READWRITER }
  | INST { INST }
  | OF { OF }
  | NODE { NODE }
  | IS { IS }
  | INVALID { INVALID }
  | WHEN { WHEN }
  | ELSE { ELSE }
  | STOP { STOP }
  | PRINTF { PRINTF }
  | SKIP { SKIP }
  | OLD { OLD }
  | NEW { NEW }
  | UNDEFINED { UNDEFINED }
  | MUX { MUX }
  | VALIDIF { VALIDIF }
  | CMEM { CMEM }
  | SMEM { SMEM }
  | MPORT { MPORT }
  | INFER { INFER }
  | READ { READ }
  | WRITE { WRITE }
  | RDWR { RDWR }
  ;

// Parentheses are added as part of name because semantics require no space between primop and open parentheses
// (And ANTLR either ignores whitespace or considers it everywhere)
primop
  : ADD { ADD }
  | SUB { SUB }
  | MUL { MUL }
  | DIV { DIV }
  | REM { REM }
  | LT { LT }
  | LEQ { LEQ }
  | GT { GT }
  | GEQ { GEQ }
  | EQ { EQ }
  | NEQ { NEQ }
  | PAD { PAD }
  | ASUINT { ASUINT }
  | ASSINT { ASSINT }
  | ASCLOCK { ASCLOCK }
  | SHL { SHL }
  | SHR { SHR }
  | DSHL { DSHL }
  | DSHR { DSHR }
  | CVT { CVT }
  | NEG { NEG }
  | NOT { NOT }
  | AND { AND }
  | OR { OR }
  | XOR { XOR }
  | ANDR { ANDR }
  | ORR { ORR }
  | XORR { XORR }
  | CAT { CAT }
  | BITS { BITS }
  | HEAD { HEAD }
  | TAIL { TAIL }
  | ASFIXEDPOINT { ASFIXEDPOINT }
  | BPSHL { BPSHL }
  | BPSHR { BPSHR }
  | BPSET { BPSET }
  ;

