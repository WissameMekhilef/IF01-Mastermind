compile : mastermind.ml listCouleur.ml
	ocamlc -c listCouleur.ml
	ocamlc -c mastermind.ml
	ocamlc -o mastermind listCouleur.cmo mastermind.cmo
	ocamlc -i listCouleur.ml > listCouleur.mli
	ocamlc -i mastermind.ml > mastermind.mli
run : mastermind
	./mastermind
pdf : rapport.tex
	pdflatex rapport.tex
	rm rapport.lof
	rm rapport.aux
	rm rapport.log
	rm rapport.toc
manuel : manuel.tex
	pdflatex manuel.tex
	rm manuel.tex
	rm manuel.aux
	rm manuel.log
	rm mauel.toc