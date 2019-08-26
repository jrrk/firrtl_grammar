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

%token Id
%token RelaxedId
%token UnsignedInt
%token SignedInt
%token StringLit
%token HexLit
%token DoubleLit
%token RawString
%token MODULE
%token EXTMODULE
%token CIRCUIT
%token DEFNAME PARAMETER
%token INPUT OUTPUT FLIP 
%token UInt SInt Fixed Clock Analog
%token LCURLY RCURLY LBRACK RBRACK COLON LESS GREATER EQUALS
%token <token option> TOPT
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
%type <token> circuit
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
  : { TOPT None }
  | defname { TOPT (Some $1) }
  
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
  : { TOPT None }
  | FLIP { TOPT (Some FLIP) }
  
defname
  : DEFNAME EQUALS id { TUPLE3(DEFNAME,EQUALS,$3) }
  ;

parameter
  : PARAMETER id EQUALS intLit { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  | PARAMETER id EQUALS stringLit { TUPLE4(PARAMETER,$2,EQUALS, $4) }
  | PARAMETER id EQUALS DoubleLit { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  | PARAMETER id EQUALS RawString { TUPLE4(PARAMETER,$2,EQUALS,$4) }
  ;

moduleBlock
  : simple_stmt_lst { $1 }
  ;

simple_stmt_lst
  : { [] }
  | simple_stmt_lst simple_stmt { $2 :: $1 }

simple_reset0:  'reset' '=>' '(' exp exp ')' { }

simple_reset
	: simple_reset0 { }
	| '(' simple_reset0 ')' { }
	;

reset_block
	:  simple_reset  { }
	| '(' simple_reset ')' { }
  ;

stmt
  : 'wire' id COLON type1 { }
  | 'reg' id COLON type1 exp with_opt  { }
  | 'mem' id COLON   memField_lst { }
  | 'cmem' id COLON type1  { }
  | 'smem' id COLON type1  { }
  | mdir 'mport' id EQUALS id LBRACK exp RBRACK exp  { }
  | 'inst' id 'of' id  { }
  | 'node' id EQUALS exp  { }
  | exp '<=' exp  { }
  | exp '<-' exp  { }
  | exp 'is' 'invalid'  { }
  | when1 { }
  | 'stop(' exp exp intLit ')'  { }
  | 'printf(' exp exp StringLit exp_lst ')'  { }
  | 'skip'  { }
  | 'attach' '(' exp_lst ')'  { }
  ;

exp_lst
  : { [] }
  | exp_lst exp { $2 :: $1 }

memField_lst
  : { [] }
  | memField_lst memField { $2 :: $1 }

with_opt:
  | 'with' COLON reset_block { }
  
memField
	:  'data-type' '=>' type1 { }
	| 'depth' '=>' intLit { }
	| 'read-latency' '=>' intLit { }
	| 'write-latency' '=>' intLit { }
	| 'read-under-write' '=>' ruw { }
	| 'reader' '=>' id_lst { }
	| 'writer' '=>' id_lst { }
	| 'readwriter' '=>' id_lst { }
	;

id_lst
  : id { }
  | id id_lst { }
  
simple_stmt
  : stmt { }
  | { }
  ;

/*
    We should provide syntatctical distinction between a "moduleBody" and a "suite":
    - statements require a "suite" which means they can EITHER have a "simple statement" (one-liner) on the same line
        OR a group of one or more _indented_ statements after a new-line. A "suite" may _not_ be empty
    - modules on the other hand require a group of one or more statements without any indentation to follow "port"
        definitions. Let's call that _the_ "moduleBody". A "moduleBody" could possibly be empty
*/
suite
  : simple_stmt { }
  | simple_stmt suite { }
  ;

when1
  : 'when' exp COLON suite_opt else_when_opt { }
  ;

suite_opt
  : { None }
  | suite { Some suite }
  
when_opt
  : when1 { }
  | COLON suite_opt { }

else_when_opt
  : { }
  | 'else' when_opt { }
  
info
  : { }
  ;

mdir
  : 'infer' { }
  | 'read' { }
  | 'write' { }
  | 'rdwr' { }
  ;

ruw
  : 'old' { }
  | 'new' { }
  | 'undefined' { }
  ;

exp
  : 'UInt' intlit_opt '(' intLit ')' { }
  | 'SInt' intlit_opt '(' intLit ')' { }
  | id  { }   // Ref
  | exp '.' fieldId { }
  | exp '.' DoubleLit  { } // TODO Workaround for #470
  | exp LBRACK intLit RBRACK { }
  | exp LBRACK exp RBRACK { }
  | 'mux(' exp exp exp ')' { }
  | 'validif(' exp exp ')' { }
  | primop exp_lst intLit_lst  ')' { }
  ;

intLit_lst
  : { [] }
  | intLit_lst intLit { $2 :: $1 }
  
id
  : Id { }
  | keywordAsId { }
  ;

fieldId
  : Id { }
  | RelaxedId { }
  | UnsignedInt { }
  | keywordAsId { }
  ;

intLit
  : UnsignedInt { }
  | SignedInt { }
  | HexLit { }
  ;

stringLit
  : StringLit { StringLit $1 }
  
// Keywords that are also legal ids
keywordAsId
  : 'circuit' { }
  | 'module' { }
  | 'extmodule' { }
  | PARAMETER { }
  | 'input' { }
  | 'output' { }
  | 'UInt' { }
  | 'SInt' { }
  | 'Clock' { }
  | 'Analog' { }
  | 'Fixed' { }
  | 'flip' { }
  | 'wire' { }
  | 'reg' { }
  | 'with' { }
  | 'reset' { }
  | 'mem' { }
  | 'depth' { }
  | 'reader' { }
  | 'writer' { }
  | 'readwriter' { }
  | 'inst' { }
  | 'of' { }
  | 'node' { }
  | 'is' { }
  | 'invalid' { }
  | 'when' { }
  | 'else' { }
  | 'stop' { }
  | 'printf' { }
  | 'skip' { }
  | 'old' { }
  | 'new' { }
  | 'undefined' { }
  | 'mux' { }
  | 'validif' { }
  | 'cmem' { }
  | 'smem' { }
  | 'mport' { }
  | 'infer' { }
  | 'read' { }
  | 'write' { }
  | 'rdwr' { }
  ;

// Parentheses are added as part of name because semantics require no space between primop and open parentheses
// (And ANTLR either ignores whitespace or considers it everywhere)
primop
  : 'add(' { }
  | 'sub(' { }
  | 'mul(' { }
  | 'div(' { }
  | 'rem(' { }
  | 'lt(' { }
  | 'leq(' { }
  | 'gt(' { }
  | 'geq(' { }
  | 'eq(' { }
  | 'neq(' { }
  | 'pad(' { }
  | 'asUInt(' { }
  | 'asSInt(' { }
  | 'asClock(' { }
  | 'shl(' { }
  | 'shr(' { }
  | 'dshl(' { }
  | 'dshr(' { }
  | 'cvt(' { }
  | 'neg(' { }
  | 'not(' { }
  | 'and(' { }
  | 'or(' { }
  | 'xor(' { }
  | 'andr(' { }
  | 'orr(' { }
  | 'xorr(' { }
  | 'cat(' { }
  | 'bits(' { }
  | 'head(' { }
  | 'tail(' { }
  | 'asFixedPoint(' { }
  | 'bpshl(' { }
  | 'bpshr(' { }
  | 'bpset(' { }
  ;

