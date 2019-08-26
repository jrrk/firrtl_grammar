O = firrtl_grammar.mli firrtl_grammar.ml firrtl_lexer.ml firrtl_main.ml

firrtl_main: $O
	ocamlc -o $@ $O

firrtl_grammar.ml firrtl_grammar.mli: firrtl_grammar.mly
	ocamlyacc -v $<

firrtl_lexer.ml: firrtl_lexer.mll
	ocamllex $<

