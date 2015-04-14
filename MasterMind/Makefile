compile : mastermind.ml listCouleur.ml
	rm *.cmo
	rm mastermind
	rm *.mli
	rm *.cmi
	ocamlc -c listCouleur.ml
	ocamlc -c mastermind.ml
	ocamlc -o mastermind listCouleur.cmo mastermind.cmo
	ocamlc -i listCouleur.ml > listCouleur.mli
	ocamlc -i mastermind.ml > mastermind.mli
run : mastermind
	./mastermind
pdf : rapport.tex
	pdflatex rapport.tex