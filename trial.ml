open Firrtl_grammar

let othexp = ref None

let showflip = function
| (OUTPUT,FLIP) -> "input"
| (OUTPUT,NOFLIP) -> "output"
| (INPUT,NOFLIP) -> "input"
| (INPUT,FLIP) -> "output"
| (_,_) -> "???"

let showio fd delim = function
   | TUPLE4 (INPUT, Id id, COLON, Clock) -> Printf.fprintf fd "%s  input         %s" !delim id
   | TUPLE4 (INPUT, RESET, COLON, TUPLE2 (UInt, BRAKET 1)) -> Printf.fprintf fd "%s  input         reset" !delim
   | TUPLE4 (dir, Id recid, COLON, TUPLE3 (LBRACE, TLIST iolst, RBRACE)) ->
       List.iter (function
           | TUPLE4 (flip, Id fieldid, COLON, TUPLE2 (UInt, BRAKET 1)) ->
	        Printf.fprintf fd "%s  %s          %s_%s" !delim (showflip (dir,flip)) recid fieldid;
           | TUPLE4 (flip, Id fieldid, COLON, TUPLE2 (UInt, BRAKET wid)) ->
	        Printf.fprintf fd "%s  %s  [%d:0]  %s_%s" !delim (showflip (dir,flip)) (wid-1) recid fieldid;
           | _ -> Printf.fprintf fd "%s???" !delim) iolst
   | _ -> Printf.fprintf fd "%s???" !delim

let showdecl fd = function
   | TUPLE4 (CMEM, Id cmem, COLON, TUPLE4 (TUPLE2 (UInt, BRAKET lft), LBRACK, UnsignedInt rght, RBRACK)) ->
       Printf.fprintf fd "  reg [%d:0] %s [0:%d];\n" (lft-1) cmem (rght-1)
   | _ -> ()

let vop = function
   | AND -> " & "
   | NEQ -> " != "
   | _ -> " ??? "

let rec showexp fd = function
   | Id id -> id
   | TUPLE3 (Id lft, DOT, Id rght) -> lft^"."^rght
   | TUPLE3 ((AND|NEQ) as op, TLIST exprlst, TLIST []) -> String.concat (vop op) (List.map (showexp fd) exprlst)
   | TUPLE5 (UInt, BRAKET n, LPAREN, HexLit h, RPAREN) -> string_of_int n^"'h"^h
   | TUPLE5 (MUX, sel, lft, rght, RPAREN) -> showexp fd sel^" ? "^showexp fd lft^" : "^showexp fd rght
   | oth -> othexp := Some oth; failwith "showexp"
   
let showcont fd = function
   | TUPLE4 (NODE, Id lft, EQUALS, expr) ->
       Printf.fprintf fd "  assign %s = %s;\n" lft (showexp fd expr)
   | _ -> ()

let showwhen fd = function
   | TUPLE4 (WHEN, Id ev, COLON, TLIST evlst) -> List.iter (function 
       | TUPLE3 (BECOMES2, Id lft, expr) ->
       Printf.fprintf fd "  %s <= %s;\n" lft (showexp fd expr)
       | _ -> Printf.fprintf fd "ev ???\n") evlst
   | _ -> ()

let trial fd portlst stmtlst =
  Printf.fprintf fd "module RegisterFile(";
  let delim = ref "\n" in List.iter (fun itm -> showio fd delim itm; delim := ",\n") portlst;
  Printf.fprintf fd "\n);\n";
  List.iter (fun itm -> showdecl fd itm) stmtlst;
  List.iter (fun itm -> showcont fd itm) stmtlst;
  List.iter (fun itm -> showwhen fd itm) stmtlst;
  Printf.fprintf fd "  reg [31:0] _RAND_0;\n";
  Printf.fprintf fd "  wire [31:0] regfile__T_39_data;\n";
  Printf.fprintf fd "  wire [4:0] regfile__T_39_addr;\n";
  Printf.fprintf fd "  wire [31:0] regfile__T_44_data;\n";
  Printf.fprintf fd "  wire [4:0] regfile__T_44_addr;\n";
  Printf.fprintf fd "  wire [31:0] regfile__T_49_data;\n";
  Printf.fprintf fd "  wire [4:0] regfile__T_49_addr;\n";
  Printf.fprintf fd "  wire [31:0] regfile__T_32_data;\n";
  Printf.fprintf fd "  wire [4:0] regfile__T_32_addr;\n";
  Printf.fprintf fd "  wire  regfile__T_32_mask;\n";
  Printf.fprintf fd "  wire  regfile__T_32_en;\n";
  Printf.fprintf fd "  wire [31:0] regfile__T_36_data;\n";
  Printf.fprintf fd "  wire [4:0] regfile__T_36_addr;\n";
  Printf.fprintf fd "  wire  regfile__T_36_mask;\n";
  Printf.fprintf fd "  wire  regfile__T_36_en;\n";
  Printf.fprintf fd "  wire  _T_30;\n";
  Printf.fprintf fd "  wire  _T_34;\n";
  Printf.fprintf fd "  wire  _T_38;\n";
  Printf.fprintf fd "  wire  _T_43;\n";
  Printf.fprintf fd "`ifdef RANDOMIZE_GARBAGE_ASSIGN\n";
  Printf.fprintf fd "`define RANDOMIZE\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "`ifdef RANDOMIZE_INVALID_ASSIGN\n";
  Printf.fprintf fd "`define RANDOMIZE\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "`ifdef RANDOMIZE_REG_INIT\n";
  Printf.fprintf fd "`define RANDOMIZE\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "`ifdef RANDOMIZE_MEM_INIT\n";
  Printf.fprintf fd "`define RANDOMIZE\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "`ifndef RANDOM\n";
  Printf.fprintf fd "`define RANDOM $random\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "`ifdef RANDOMIZE\n";
  Printf.fprintf fd "  integer initvar;\n";
  Printf.fprintf fd "  initial begin\n";
  Printf.fprintf fd "    `ifdef INIT_RANDOM\n";
  Printf.fprintf fd "      `INIT_RANDOM\n";
  Printf.fprintf fd "    `endif\n";
  Printf.fprintf fd "    `ifndef VERILATOR\n";
  Printf.fprintf fd "      #0.002 begin end\n";
  Printf.fprintf fd "    `endif\n";
  Printf.fprintf fd "  _RAND_0 = {1{`RANDOM}};\n";
  Printf.fprintf fd "  `ifdef RANDOMIZE_MEM_INIT\n";
  Printf.fprintf fd "  for (initvar = 0; initvar < 32; initvar = initvar+1)\n";
  Printf.fprintf fd "    regfile[initvar] = _RAND_0[31:0];\n";
  Printf.fprintf fd "  `endif  <= regfile__T_32_data;\n";
  Printf.fprintf fd "    end\n";
  Printf.fprintf fd "    if(regfile__T_36_en & regfile__T_36_mask) begin\n";
  Printf.fprintf fd "      regfile[regfile__T_36_addr] <= regfile__T_36_data;\n";
  Printf.fprintf fd "    end\n";
  Printf.fprintf fd "  end\n";
  Printf.fprintf fd "`endif\n";
  Printf.fprintf fd "endmodule\n";
  Printf.fprintf fd "\n";
  ()


