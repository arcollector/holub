test:
	ocamlbuild test.byte

myRegexLexer:
	ocamlbuild myRegexLexer.byte

myRegexParser:
	ocamlbuild myRegexParser.byte

myRegexSpec: myRegexLexer myRegexParser
	ocamlbuild -pkg ounit2 myRegexSpecLexer.byte
	ocamlbuild -pkg ounit2 myRegexSpecCcl.byte
	ocamlbuild -pkg ounit2 myRegexSpecChar.byte
	ocamlbuild -pkg ounit2 myRegexSpecFactor.byte
	ocamlbuild -pkg ounit2 myRegexSpecExpr.byte
	ocamlbuild -pkg ounit2 myRegexSpecMachine.byte
	./myRegexSpecLexer.byte && \
	./myRegexSpecCcl.byte && \
	./myRegexSpecChar.byte && \
	./myRegexSpecFactor.byte && \
	./myRegexSpecExpr.byte && \
	./myRegexSpecMachine.byte


myRegexRunner:
	ocamlbuild myRegexRunner.byte

myRegexSpecRunner: myRegexRunner
	ocamlbuild -pkg ounit2 myRegexSpecRunner.byte
	./myRegexSpecRunner.byte

egrep:
	ocamlbuild egrep.byte

nfa_to_dfa:
	ocamlbuild nfa_to_dfa.byte

nfa_to_dfa_spec: nfa_to_dfa
	ocamlbuild -pkg ounit2 nfa_to_dfa_spec.byte
	./nfa_to_dfa_spec.byte

dfa_minimization: nfa_to_dfa
	ocamlbuild dfa_minimization.byte
	./dfa_minimization.byte
