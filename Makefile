# Makefile

update: data-format.pdf
	git push  git@github.com:jmakino/Particle-Stream-Data-Format.git
	git commit -a -m "Kobe update"
	git push git@github.com:jmakino/Particle-Stream-Data-Format.git

data-format.pdf: data-format.tex
	latex data-format
	latex data-format
	dvipdfmx data-format

