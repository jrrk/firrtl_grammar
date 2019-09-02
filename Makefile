Y = ocamlyacc -v
# Y = menhir --trace
O = firrtl_grammar.mli firrtl_lexer.ml firrtl_grammar.ml firrtl_dump.ml firrtl_main.ml
TARGET = Accumulator
all: firrtl_main firrtl_opt firrtl_top

firrtl_main: $O firrtl_args.ml
	ocamlc -g -o $@ $O firrtl_args.ml

firrtl_opt: $O firrtl_args.ml
	ocamlopt -g -o $@ $O firrtl_args.ml

firrtl_top: $O
	ocamlmktop -o $@ $O

firrtl_grammar.ml firrtl_grammar.mli: firrtl_grammar.mly
	$Y $<

firrtl_lexer.ml: firrtl_lexer.mll
	ocamllex $<

regression:
	(cd fir; rm -f logfile ; for i in *.fir; do ../firrtl_opt $$i |& tee -a logfile;done)

debug:
	echo -e open Firrtl_grammar \\nopen Firrtl_main \\nopen Firrtl_dump \\nlet _ = iterate \"fir/${TARGET}.fir\" \"\"\;\; \\n > .ocamlinit
	./firrtl_top
