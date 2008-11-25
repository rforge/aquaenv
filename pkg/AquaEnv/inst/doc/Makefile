all: vignette_AquaEnv vignette_AquaEnv-PrivateFunctions clean


vignette_AquaEnv:
	pdflatex AquaEnv.tex
	bibtex AquaEnv
	pdflatex AquaEnv.tex
	pdflatex AquaEnv.tex


vignette_AquaEnv-PrivateFunctions:
	pdflatex AquaEnv-PrivateFunctions.tex


clean:
	rm -f *~
	rm -f AquaEnv-PrivateFunctions.out
	rm -f AquaEnv-PrivateFunctions.log
	rm -f AquaEnv-PrivateFunctions.aux
	rm -f AquaEnv.aux
	rm -f AquaEnv.bbl
	rm -f AquaEnv.blg
	rm -f AquaEnv.log
	rm -f AquaEnv.out
	rm -f AquaEnv.toc
	mv AquaEnv-PrivateFunctions.pdf dummy.pdf
	mv AquaEnv-PrivateFunctions.tex dummy.tex
	rm -f AquaEnv-*
	mv dummy.pdf AquaEnv-PrivateFunctions.pdf 
	mv dummy.tex AquaEnv-PrivateFunctions.tex 
	rm -f Rplots.pdf
	rm -f test*