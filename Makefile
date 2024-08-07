build:
	cd mini-witherable ; stack build
	cd modern-map-merge ; stack test

documents: rationale/blogpost.html \
           rationale/summary.html \
           README.html \
           CHANGELOG.html \
           mini-witherable/README.html \
           modern-map-merge/src-extra/README.html \
           modern-map-merge/src-listmap/README.html \
           modern-map-merge/src-maplike/README.html \
           modern-map-merge/src-modernmapmerge/README.html

%.html: %.md
	markdown $< >$@
