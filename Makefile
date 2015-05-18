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
	@xelatex -shell-escape ${FILEBASE}.tex


tex: ${FILEBASE}.tex
${FILEBASE}.tex: ${FILEBASE}.lhs ${MINT} Makefile mint.hs
	@echo "Making tex"
	@pandoc -F ./fuse-code.hs -F ./mint.hs -w latex --no-highlight -t beamer -H ${MINT} -H ${FONT} ${LHS} | python fragile-frame.py > ${FILEBASE}.tex


run:
	runhaskell main.hs

clean:
	-@rm -Rf *.aux *.log *.nav *.out *.snm *.toc *.vrb *.hi *.o *.pyg _minted*
	-@rm -Rf main

