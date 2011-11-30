mkhtml: Files.hs Lex.hs LineTypes.hs Snip.hs Paragraphs.hs GenerateHTML.hs mkhtml.hs
	ghc --make -O2 -Wall mkhtml 

debug:
	ghc --make -O2 -Wall -prof mkhtml

clean:
	rm *.o *.hi mkhtml