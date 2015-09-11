# This is to make the expansion in clean work.
SHELL=/bin/bash

FILEBASE:=$(shell find . -name "Bahug101Lec*.lhs" | cut -c 3- \
									| cut -d '.' -f 1)
SUPPORT=../compile

# FILEBASE=Bahug101-Lec1
LHS=${FILEBASE}.lhs

FUSEFILT:=${SUPPORT}/fuse-code.hs
MINTFILT:=${SUPPORT}/mint.hs

MINTOPTS:=${SUPPORT}/mint.ltx
FONTOPTS:=${SUPPORT}/font.ltx


#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: pdf tex run clean

pdf: ${FILEBASE}.pdf
${FILEBASE}.pdf: ${FILEBASE}.lhs ${FILEBASE}.tex Makefile
	@echo "Making PDF"
	@xelatex -shell-escape ${FILEBASE}.tex


tex: ${FILEBASE}.tex
${FILEBASE}.tex: ${FILEBASE}.lhs ${FONTOPTS} ${MINTOPTS} ${MINTFILT} ${FUSEFILT} Makefile
	@echo "Making tex"
	@pandoc -F ${FUSEFILT} -F ${MINTFILT} -w latex --no-highlight \
     -t beamer -H ${MINTOPTS} -H ${FONTOPTS} ${LHS} | \
     python ../compile/fragile-frame.py > ${FILEBASE}.tex

run:
	runhaskell main.hs


cleantex:
	-@rm -Rf *.aux *.log *.nav *.out *.snm *.toc *.vrb *.tex\
		_minted-${FILEBASE}


clean: cleantex
	-@rm -Rf *.hi *.o *.pyg
	-@rm -Rf main
