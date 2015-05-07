all: prospectus.pdf
clean: cleanup; rm -f prospectus.pdf
cleanup:; rm -f *.aux *.dvi *.log *~
%.pdf: %.tex; pdflatex -interaction batchmode $^
