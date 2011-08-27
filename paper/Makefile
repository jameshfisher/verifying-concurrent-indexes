
TOPDF = wkhtmltopdf --print-media-type --margin-top 2cm --margin-right 2cm --margin-bottom 2cm --margin-left 2cm --page-size A4

all:
	xsltproc --xinclude transform.xsl paper.html > out.html


report.pdf:
	$(TOPDF) out.html report.pdf
