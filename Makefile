all: 
	ocamlbuild -use-ocamlfind main.byte

install: all
	cp main.byte /usr/local/bin/doc-runorg 

