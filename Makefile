build:
	cd current-approach ; stack build
	cd prospective-approach ; stack build

documents: rationale/blogpost.html \
           rationale/summary.html \
           README.html \
           CHANGELOG.html \
           current-approach/src/README.html \
           prospective-approach/src-extra/README.html \
           prospective-approach/src-listmap/README.html \
           prospective-approach/src-maplike/README.html \
           prospective-approach/src-modernmapmerge/README.html \
           prospective-approach/src-witherable/README.html

%.html: %.md
	markdown $< >$@
