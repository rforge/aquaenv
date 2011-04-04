all: vignette_AquaEnv vignette_AquaEnv-PrivateFunctions vignette_AquaEnv-ConstantsAndFormulae


vignette_AquaEnv:
	pdflatex AquaEnv.tex
	bibtex AquaEnv
	pdflatex AquaEnv.tex
	pdflatex AquaEnv.tex

vignette_AquaEnv-PrivateFunctions:
	pdflatex AquaEnv-PrivateFunctions.tex

vignette_AquaEnv-ConstantsAndFormulae:
	pdflatex AquaEnv-ConstantsAndFormulae.tex
	bibtex AquaEnv-ConstantsAndFormulae
	pdflatex AquaEnv-ConstantsAndFormulae.tex
	pdflatex AquaEnv-ConstantsAndFormulae.tex

clean:
	rm -f *~
	rm -f AquaEnv.aux
	rm -f AquaEnv.bbl
	rm -f AquaEnv.blg
	rm -f AquaEnv.log
	rm -f AquaEnv.out
	rm -f AquaEnv.toc
	mv AquaEnv-PrivateFunctions.pdf dummy.pdf
	mv AquaEnv-PrivateFunctions.tex dummy.tex
	mv AquaEnv-ConstantsAndFormulae.pdf dummy1.pdf
	mv AquaEnv-ConstantsAndFormulae.tex dummy1.tex
	rm -f AquaEnv-*
	mv dummy.pdf AquaEnv-PrivateFunctions.pdf 
	mv dummy.tex AquaEnv-PrivateFunctions.tex 
	mv dummy1.pdf AquaEnv-ConstantsAndFormulae.pdf 
	mv dummy1.tex AquaEnv-ConstantsAndFormulae.tex 
	rm -f Rplots.pdf
	rm -f test*
	rm -f q.log
	rm -f *.bak