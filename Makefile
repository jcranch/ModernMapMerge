build:
	stack build

documents: rationale/blogpost.html README.html

%.html: %.md
	markdown $< >$@
