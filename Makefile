lexer_o:
	ocamlbuild -use-ocamlfind lexer_o.native

test_lexer_o: lexer_o
	ocamlbuild -use-ocamlfind _test_/lexer_o_spec.native

test_lexer_r:
	ocamlbuild -use-ocamlfind lexer_r.native
	ocamlbuild -use-ocamlfind _test_/lexer_r_spec.native

parser:
	ocamlbuild -use-ocamlfind parser.native

test_parser: parser
	ocamlbuild -use-ocamlfind _test_/parser_spec.native
