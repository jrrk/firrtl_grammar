Y = ocamlyacc -v
Y = menhir --trace
O = firrtl_grammar.mli firrtl_lexer.ml firrtl_grammar.ml firrtl_main.ml

firrtl_main: $O
	ocamlc -o $@ $O

firrtl_grammar.ml firrtl_grammar.mli: firrtl_grammar.mly
	$Y $<

firrtl_lexer.ml: firrtl_lexer.mll
	ocamllex $<

