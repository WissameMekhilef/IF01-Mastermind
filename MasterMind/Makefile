compile : mastermind.ml listCouleur.ml
	ocamlc -i listCouleur.ml > listCouleur.mli
	ocamlc -i mastermind.ml > mastermind.mli
	ocamlc -c listCouleur.ml
	ocamlc -c mastermind.ml
	ocamlc -o mastermind listCouleur.cmo mastermind.cmo
run : mastermind
	./mastermind
pdf : rapport.tex
	pdflatex rapport.tex