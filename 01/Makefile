PACKAGES=-package core -package stdio
OCAMLFIND=ocamlfind
OCAMLFIND_ARGS=ocamlc -thread -linkpkg $(PACKAGES)

all: solve.byte

%.byte: %.ml
	$(OCAMLFIND) $(OCAMLFIND_ARGS) $< -o $@

clean:
	rm *.byte *.cmo *.cmi

.PHONY: all clean
