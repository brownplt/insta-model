TALK=talk.ss
RACO=/Applications/Racket-8.3-CS/bin/raco

all: full

full: fast bib
	pdflatex paper
	pdflatex paper

fast:
	pdflatex paper

bib:
	biber paper

compiletalk:
	@${RACO} make -v ${TALK}

pict: compiletalk
	@${RACO} pict ${TALK}

show: compiletalk
	@${RACO} slideshow --widescreen ${TALK}

pdf: compiletalk
	@${RACO} slideshow --widescreen --condense --pdf ${TALK}

