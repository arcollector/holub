lexer_o:
	ocamlbuild -use-ocamlfind lexer_o.native

test_lexer_o: lexer_o
	ocamlbuild -use-ocamlfind _test_/lexer_o_spec.native

test_lexer_r:
	ocamlbuild -use-ocamlfind lexer_r.native
	ocamlbuild -use-ocamlfind _test_/lexer_r_spec.native

parser_rec:
	ocamlbuild -use-ocamlfind parser_rec.native

test_parser_rec: parser_rec
	ocamlbuild -use-ocamlfind _test_/parser_rec_spec.native

parser_rec_2:
	ocamlbuild -use-ocamlfind parser_rec_2.native

test_parser_rec_2: parser_rec_2
	ocamlbuild -use-ocamlfind _test_/parser_rec_2_spec.native

ex_1:
	ocamlbuild -use-ocamlfind exercises/ex_1.native

ex_1_spec: ex_1
	ocamlbuild -use-ocamlfind -I exercises _test_/ex_1_spec.native

ex_3:
	ocamlbuild -use-ocamlfind exercises/ex_3.native

ex_3_spec: ex_1
	ocamlbuild -use-ocamlfind -I exercises _test_/ex_3_spec.native

clean:
	rm -rf *.native _build