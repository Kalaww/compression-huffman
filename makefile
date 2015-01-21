EXEC=projet
SOURCE=projet
DOC=doc

all:
	ocamlopt -c bitio.mli
	ocamlopt -c bitio.ml
	ocamlopt -c $(SOURCE).mli
	ocamlopt -c $(SOURCE).ml
	ocamlopt -o $(EXEC) bitio.cmx $(SOURCE).cmx

mli:
	ocamlopt -i $(SOURCE).ml

odoc:
	rm -rf $(DOC)
	mkdir -p $(DOC)
	ocamldoc -html -d $(DOC) -charset utf8 $(SOURCE).mli

clean:
	rm -rf $(EXEC) $(SOURCE).cmo $(SOURCE).cmi $(SOURCE).cmx $(SOURCE).o bitio.cmx bitio.cmo bitio.cmi