Y = ocamlyacc -v
# Y = menhir --trace
O = firrtl_grammar.mli firrtl_lexer.ml firrtl_grammar.ml trial.ml firrtl_main.ml

all: firrtl_main firrtl_opt firrtl_top

firrtl_main: $O firrtl_args.ml
	ocamlc -o $@ $O firrtl_args.ml

firrtl_opt: $O firrtl_args.ml
	ocamlopt -o $@ $O firrtl_args.ml

firrtl_top: $O
	ocamlmktop -o $@ $O

firrtl_grammar.ml firrtl_grammar.mli: firrtl_grammar.mly
	$Y $<

firrtl_lexer.ml: firrtl_lexer.mll
	ocamllex $<

