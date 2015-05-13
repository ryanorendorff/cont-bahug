# This is to make the expansion in clean work.
SHELL=/bin/bash

FILEBASE=ContBahug
LHS=${FILEBASE}.lhs
MINT=mint.ltx
FONT=font.ltx


#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: pdf tex run clean

pdf: ${FILEBASE}.pdf
${FILEBASE}.pdf: ${FILEBASE}.lhs ${FILEBASE}.tex Makefile
	@echo "Making PDF"
	@sed -i.bak "s/verbatim/haskellcode/g" ${FILEBASE}.tex
	@rm ${FILEBASE}.tex.bak
	@xelatex -shell-escape ${FILEBASE}.tex >> /dev/null


tex: ${FILEBASE}.tex
${FILEBASE}.tex: ${FILEBASE}.lhs ${MINT} Makefile
	@echo "Making tex"
	@pandoc -F ./pandoc-mintedcode.pl -w latex --no-highlight -t beamer -H ${MINT} -H ${FONT} ${LHS} -o ${FILEBASE}.tex

run:
	runhaskell main.hs

clean:
	-@rm -Rf *.aux *.log *.nav *.out *.snm *.toc *.vrb *.hi *.o *.pyg _minted*
	-@rm -Rf main

