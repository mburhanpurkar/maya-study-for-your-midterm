all: one two

one: 1oftvb.ml
	ocamlbuild -use-ocamlfind 1oftvb.ml.byte

two: 2oftvb.ml
	ocamlbuild -use-ocamlfind 2oftvb.ml.byte
