all:
	lualatex --shell-escape background.tex

full:
	lualatex --shell-escape background.tex
	bibtex background
	lualatex --shell-escape background.tex
	lualatex --shell-escape background.tex

clean:
	rm *~ *.aux *.bbl *.blg *.log *.toc