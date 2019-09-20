open Firrtl_lexer
open Firrtl_grammar

let cmemhash = Hashtbl.create 255

let othexp = ref None

let showflip' = function
| (OUTPUT,FLIP) -> INPUT
| (OUTPUT,NOFLIP) -> OUTPUT
| (INPUT,NOFLIP) -> INPUT
| (INPUT,FLIP) -> OUTPUT
| (_,_) -> UNDEFINED

let dirstr = function
| OUTPUT -> "output"
| INPUT -> "input "
| UNDEFINED -> "wire  "
| REG -> "reg   "
| _ -> "undef"

let idtab = Hashtbl.create 255

let showflip dir flip = dirstr(showflip' (dir,flip))

let rec subio fd delim dir recid = function
           | TUPLE4 (flip, Id fieldid, COLON, TUPLE2 (UInt, BRAKET 1)) ->
                let fid = recid^"_"^fieldid in
                let dir' = showflip' (dir, flip) in
                Hashtbl.add idtab fid (dir',1);
	        Printf.fprintf fd "%s %s\t\t%s" !delim (dirstr dir') fid;
           | TUPLE4 (flip, Id fieldid, COLON, TUPLE2 (UInt, BRAKET wid)) ->
                let fid = recid^"_"^fieldid in
                let dir' = showflip' (dir, flip) in
                Hashtbl.add idtab fid (dir',wid);
	        Printf.fprintf fd "%s %s\t[%d:0]\t%s" !delim (dirstr dir') (wid-1) fid;
           | TUPLE4 (flip, Id recid', COLON, TUPLE3 (LBRACE, TLIST iolst', RBRACE)) ->
                List.iter (subio fd delim (showflip' (dir,flip)) (recid^"_"^recid')) iolst'
           | TUPLE4 (flip, Id recid', COLON, TUPLE4 (TUPLE3 (LBRACE, TLIST iolst', RBRACE), LBRACK, ix, RBRACK)) -> 
                List.iter (subio fd delim (showflip' (dir,flip)) (recid^"_"^recid')) iolst'
           | oth -> othexp := Some oth; Printf.fprintf fd "%s??? 25" !delim

let showio fd delim = function
   | TUPLE4 (dir', Id id, COLON, Clock) ->
       Hashtbl.add idtab id (dir', 1);
       Printf.fprintf fd "%s %s\t\t%s" !delim (dirstr dir') id
   | TUPLE4 (dir', TUPLE1 kw, COLON, TUPLE2 (UInt, BRAKET 1)) ->
       let id' = asid kw in
       Hashtbl.add idtab id' (dir', 1);
       Printf.fprintf fd "%s %s\t\t%s" !delim (dirstr dir') id'
   | TUPLE4 (dir, Id recid, COLON, TUPLE3 (LBRACE, TLIST iolst, RBRACE)) ->
       List.iter (subio fd delim dir recid) iolst
   | _ -> Printf.fprintf fd "%s??? 26" !delim

let rec bits arg = 1 + (if arg > 1 then bits (arg lsr 1) else 0)

let indstr indent = String.make indent '\t'
   
let vop = function
   | AND -> " & "
   | OR -> " | "
   | XOR -> " ^ "
   | EQ -> " == "
   | NEQ -> " != "
   | GEQ -> " >= "
   | LEQ -> " <= "
   | LT -> " < "
   | GT -> " > "
   | ADD -> " + "
   | SUB -> " - "
   | MUL -> " * "
   | DIV -> " / "
   | SHL -> " << "
   | SHR -> " >> "
   | DSHL -> " << "
   | DSHR -> " << "
   | CVT -> " cvt "
   | _ -> " ??? 62 "

let mop = function
   | ASUINT -> "$unsigned"
   | ASSINT -> "$signed"
   | _ -> " ??? 67 "
   
let rec showexp = function
   | Id id -> id
   | UnsignedInt n -> string_of_int n
   | TUPLE3 (lft, DOT, rght) -> showexp lft^"_"^showexp rght
   | TUPLE3 ((SHL|SHR) as op, TLIST [expr1], TLIST [expr2]) ->
      showexp expr1^vop op^showexp expr2
   | TUPLE3 ((AND|OR|XOR|EQ|NEQ|LEQ|GEQ|LT|GT|ADD|SUB|MUL|DIV|DSHL|DSHR|CVT) as op, TLIST exprlst, TLIST []) ->
      String.concat (vop op) (List.map (showexp) exprlst)
   | TUPLE3 (CAT, TLIST exprlst, TLIST []) ->
      "{"^String.concat "," (List.map (showexp) exprlst)^"}"
   | TUPLE3 (TAIL, TLIST [expr], TLIST [UnsignedInt n]) -> showexp expr^" /* truncate "^string_of_int n^" bits(s) */ "
   | TUPLE3 (NOT, TLIST [expr], TLIST []) -> "!"^showexp expr
   | TUPLE3 ((ASUINT|ASSINT) as op, TLIST [expr], TLIST []) -> mop op^"("^showexp expr^")"
   | TUPLE5 (UInt, BRAKET n, LPAREN, HexLit h, RPAREN) -> string_of_int (4*String.length h)^"'h"^h
   | TUPLE5 (MUX, sel, lft, rght, RPAREN) -> showexp sel^" ? "^showexp lft^" : "^showexp rght
   | TUPLE4 (VALIDIF, lft, rght, RPAREN) -> "valid_if("^showexp lft^", "^showexp rght^")"
   | TUPLE3 (BITS,
      TLIST [(TUPLE3(_, DOT, _)|Id _) as id'],
      TLIST [hi; lo]) -> showexp id' ^ "[" ^ showexp hi ^":" ^showexp lo ^ "]"
   | TUPLE3 (BITS, TLIST [expr], TLIST [UnsignedInt _; UnsignedInt _]) -> "BITS(...)"
   | TUPLE4 (expr, LBRACK, ix, RBRACK) -> showexp expr^"["^showexp ix^"]"
   | TUPLE1 arg -> asid arg
   | oth -> othexp := Some oth; failwith "showexp 108"
   
let rec getwid = function
   | Id id -> if Hashtbl.mem idtab id then let (dir',wid) = Hashtbl.find idtab id in wid else 1
   | UnsignedInt n -> n
   | TUPLE3 (lft, DOT, rght) -> let id = showexp lft^"_"^showexp rght in
      if Hashtbl.mem idtab id then let (dir',wid) = Hashtbl.find idtab id in wid else 1
   | TUPLE3 (SHL, TLIST [expr1], TLIST [expr2]) ->
      let wid1 = getwid expr1 (* and wid2 = getwid expr2 *) in
      wid1
   | TUPLE3 (SHR, TLIST [expr1], TLIST [expr2]) ->
      let wid1 = getwid expr1 (* and wid2 = getwid expr2 *) in
      wid1
   | TUPLE3 ((AND|OR|XOR), TLIST exprlst, TLIST []) ->
      List.fold_right (max) (List.map getwid (List.tl exprlst)) (getwid (List.hd exprlst))
   | TUPLE3 ((EQ|NEQ|LEQ|GEQ|LT|GT), TLIST exprlst, TLIST []) -> 1
   | TUPLE3 ((ADD|SUB), TLIST exprlst, TLIST []) ->
      1 + List.fold_right (max) (List.map getwid (List.tl exprlst)) (getwid (List.hd exprlst))
   | TUPLE3 ((MUL|DIV|DSHL|DSHR|CVT), TLIST exprlst, TLIST []) ->
      List.fold_right (max) (List.map getwid (List.tl exprlst)) (getwid (List.hd exprlst))
   | TUPLE3 (CAT, TLIST exprlst, TLIST []) ->
      List.fold_right (+) (List.map getwid (List.tl exprlst)) (getwid (List.hd exprlst))
   | TUPLE3 (TAIL, TLIST [expr], TLIST [UnsignedInt n]) -> getwid expr - n
   | TUPLE3 (NOT, TLIST [expr], TLIST []) -> 1
   | TUPLE3 ((ASUINT|ASSINT), TLIST [expr], TLIST []) -> getwid expr
   | TUPLE5 (UInt, BRAKET n, LPAREN, HexLit h, RPAREN) -> 4*String.length h
   | TUPLE5 (MUX, sel, lft, rght, RPAREN) -> max (getwid lft) (getwid rght)
   | TUPLE3 (BITS,
      TLIST [expr],
      TLIST [UnsignedInt hi; UnsignedInt lo]) -> hi-lo+1
   | TUPLE4 (expr, LBRACK, ix, RBRACK) -> getwid expr
   | TUPLE1 arg -> getwid (Id (asid arg))
   | TUPLE4 (VALIDIF, _, _, RPAREN) -> 1
   | oth -> othexp := Some oth; failwith "showexp 140"
   
let showdecl inner fd indent = function
   | TUPLE4 (NODE, Id lft, EQUALS, expr) ->
       let wid = getwid expr in
       Hashtbl.add idtab lft ((if inner then REG else UNDEFINED),wid);
       Printf.fprintf fd "%s%s [%d:0]\t%s;\n" (indstr indent) (if inner then "reg   " else "  wire") (wid-1) lft
   | TUPLE4 (CMEM, Id cmem, COLON, TUPLE4 (TUPLE2 (UInt, BRAKET lft), LBRACK, UnsignedInt rght, RBRACK)) ->
      Hashtbl.add cmemhash cmem (lft,rght);
      Printf.fprintf fd "  reg [%d:0] %s [0:%d];\n" (lft-1) cmem (rght-1)
   | TUPLE9 (INFER, MPORT, Id lft, EQUALS, Id cmem, LBRACK, expr, RBRACK, Id clock) ->
       let (cwid,cdim) = Hashtbl.find cmemhash cmem in
       Printf.fprintf fd "  wire [%d:0]\t%s_%s_data;\n" (cwid-1) cmem lft;
       Printf.fprintf fd "  wire [%d:0]\t%s_%s_addr;\n" (bits(cdim-1)-1) cmem lft;
   | TUPLE3 (expr, IS, INVALID) ->
       Printf.fprintf fd "  wire %s;\n" (showexp expr)
   | TUPLE6 (REG, Id id, COLON, TUPLE2 (UInt, BRAKET wid), Id clock, _) ->
       Hashtbl.add idtab id (REG,wid);
       Printf.fprintf fd "  reg [%d:0]\t%s;\n" (wid-1) id
   | TUPLE6 (REG, Id id, COLON, TUPLE2 (UInt, TNone), Id clock, _) ->
       Hashtbl.add idtab id (REG,1);
       Printf.fprintf fd "  reg\t%s;\n" id
   | TUPLE4 (WIRE, Id id, COLON, TUPLE2 (UInt, BRAKET wid)) ->
       Printf.fprintf fd "  wire [%d:0]\t%s;\n" (wid-1) id
   | TUPLE4 (WIRE, Id id, COLON, TUPLE4 (TUPLE2 (UInt, _), LBRACK, UnsignedInt ix, RBRACK)) ->
       Printf.fprintf fd "  wire %s;\n" id
   | TUPLE4 (WIRE, Id recid, COLON, TUPLE3 (LBRACE, TLIST reclst, RBRACE)) ->
       let delim = ref "\n" in
       List.iter (fun itm -> subio fd delim WIRE recid itm; delim := ";\n") reclst;
       Printf.fprintf fd ";\n"
   | TUPLE4 (INST, Id id, OF, Id kind) -> 
       Printf.fprintf fd "%s %s();\n" kind id
   | oth -> othexp := Some oth; failwith "showdecl 40"

let dfilt = function
    | TUPLE4 (CMEM,_,_,_) -> true
    | TUPLE4 (NODE,_,_,_) -> true
    | TUPLE9 (INFER, MPORT, _, _, _, _, _, _, _) -> true
    | TUPLE3 ((Id _ | TUPLE3 (_, DOT, _) | TUPLE1 _), IS, INVALID) -> true
    | TUPLE6 (REG, Id _, COLON, _, Id _, _) -> true
    | TUPLE4 (WIRE, Id _, COLON, _) -> true
    | TUPLE4 (INST, Id _, OF, Id _) -> true
    | _ -> false

let rec when' fd clk indent = function 
       | TUPLE3 (BECOMES2, lft, expr) ->
           Printf.fprintf fd "%s%s <= %s;\n" (indstr indent) (showexp lft) (showexp expr)
       | TUPLE9 (INFER, MPORT, Id lft, EQUALS, Id cmem, LBRACK, expr, RBRACK, Id clock) ->
           let _ = match !clk with
             | None ->
                 Printf.fprintf fd "  always @(posedge %s) begin\n" clock; clk := Some clock
             | Some x when x <> clock ->
                 Printf.fprintf fd "  end\n";
                 Printf.fprintf fd "  always @(posedge %s) begin\n" clock; clk := Some clock
             | _ -> () in
           Printf.fprintf fd "      if(%s_%s_en & %s_%s_mask) begin\n" cmem lft cmem lft;
           Printf.fprintf fd "        %s[%s_%s_addr] <= %s_%s_data;\n" cmem cmem lft cmem lft;
           Printf.fprintf fd "    end\n";
       | TUPLE4 (NODE, Id lft, EQUALS, expr) ->
           Printf.fprintf fd "%s%s = %s;\n" (indstr indent) lft (showexp expr)
       | TUPLE6 (PRINTF, clock, TUPLE5 _, StringLit _, TLIST itmlst, RPAREN) ->
           Printf.fprintf fd "%s$display(...);\n" (indstr indent) ;
       | TUPLE5 (STOP, clock, TUPLE5 _, UnsignedInt 1, RPAREN) ->
           Printf.fprintf fd "%s$stop;\n" (indstr indent) ;
       | TUPLE4 (WIRE, Id _, COLON, _) -> ()
       | SKIP -> ()
       | TUPLE4 (WHEN, ev, COLON, TLIST evlst) ->
	    Printf.fprintf fd "%sif (%s)\n%s\tbegin\n" (indstr indent) (showexp ev) (indstr indent);
	    List.iter (showdecl true fd (indent+1)) (List.filter dfilt evlst);
	    List.iter (when' fd clk (indent+1)) evlst;
	    Printf.fprintf fd "%s\tend\n" (indstr indent) ;
       | TUPLE7 (WHEN, ev, COLON, TLIST evlst, ELSE, COLON, TLIST elslst) ->
	    Printf.fprintf fd "%sif (%s)\n\t\tbegin\n" (indstr indent) (showexp ev);
	    List.iter (when' fd clk (indent+1)) evlst;
	    Printf.fprintf fd "%s\tend\n\telse\n\t\tbegin\n" (indstr indent) ;
	    List.iter (when' fd clk (indent+1)) elslst;
	    Printf.fprintf fd "%s\tend\n" (indstr indent);
       | TUPLE4 (INST, Id id, OF, Id kind) -> 
            Printf.fprintf fd "%s%s %s();\n" (indstr indent) kind id
       | TUPLE3 (Id _, IS, INVALID) -> ()
       | oth -> othexp := Some oth; failwith "showwhen 95"

and showbody fd clk = function
   | TUPLE4 (NODE, Id lft, EQUALS, expr) ->
       Printf.fprintf fd "  assign %s = %s;\n" lft (showexp expr)
   | TUPLE9 (INFER, MPORT, Id lft, EQUALS, Id cmem, LBRACK, expr, RBRACK, Id clock) ->
       Printf.fprintf fd "  assign %s_%s_addr = %s;\n" cmem lft (showexp expr);
       Printf.fprintf fd "  assign %s_%s_data = %s[%s_%s_addr];\n" cmem lft cmem cmem lft
   | TUPLE3 (BECOMES1, lft, rght) ->
       Printf.fprintf fd "  assign %s = %s;\n" (showexp lft) (showexp rght)
   | TUPLE3 (BECOMES2, lft, rght) ->
       Printf.fprintf fd "  assign %s = %s;\n" (showexp lft) (showexp rght)
   | TUPLE4 ((CMEM|WIRE|INST),_,_,_) -> ()
   | TUPLE3 (_, IS, INVALID) -> ()
   | TUPLE6 (REG, Id id, COLON, TUPLE2 (UInt, BRAKET wid), Id clock, TUPLE3 (WITH, COLON,
      TUPLE3 (LPAREN, TUPLE6 (RESET, CONNECTS, LPAREN, reset, rval, RPAREN), RPAREN))) ->
       Printf.fprintf fd "  always @(posedge %s) if (%s) %s <= %s; else\n" clock (showexp reset) id (showexp rval)
   | TUPLE6 (REG, Id id, COLON, TUPLE2 (UInt, (BRAKET 1 | TNone)), Id clock, TNone) ->
       Printf.fprintf fd "  always @(posedge %s)\n" clock
   | TUPLE4 (WHEN, ev, COLON, TLIST evlst) as evnt -> when' fd clk 1 evnt
   | TUPLE7 (WHEN, ev, COLON, TLIST evlst, ELSE, COLON, TLIST elslst) as evnt -> when' fd clk 1 evnt
   | TUPLE4 (MEM, Id stack_mem, COLON, TLIST attrlst) -> ()
   | oth -> othexp := Some oth; failwith "showcont 236"

let comb fd ev =
        Printf.fprintf fd "  always @*\n\tbegin\n\tif (%s)\n\t\tbegin\n" (showexp ev);
        Printf.fprintf fd "\t\tend\n\tend\n"

let dump nam portlst stmtlst =
  let decllst = List.filter dfilt stmtlst in
  let fd = open_out (nam^".sv") in
  Printf.fprintf fd "module %s(" nam;
  let delim = ref "\n" in
  List.iter (fun itm -> showio fd delim itm; delim := ",\n") portlst;
  Printf.fprintf fd "\n);\n";
  List.iter (showdecl false fd 0) decllst;
  let clk = ref None in List.iter (showbody fd clk) stmtlst;
  if !clk <> None then Printf.fprintf fd "  end\n";
  Printf.fprintf fd "endmodule\n";
  Hashtbl.iter (fun fid (dir',wid) ->
    Printf.fprintf fd "/* %s\t%d\t%s */\n" (dirstr dir') wid fid) idtab;
  close_out fd
