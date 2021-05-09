test_lexer_o:
	ocamlbuild -use-ocamlfind lexer_o.native
	ocamlbuild -use-ocamlfind _test_/lexer_o_spec.native

test_lexer_r:
	ocamlbuild -use-ocamlfind lexer_r.native
	ocamlbuild -use-ocamlfind _test_/lexer_r_spec.native