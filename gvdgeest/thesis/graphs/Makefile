TARGETS = $(patsubst %.dot,%.pdf,$(wildcard *.dot))

all : $(TARGETS)

%.pdf : %.ps
	epstopdf $*.ps

%.pdf : %.eps
	epstopdf $*.eps

%.ps : %.dot
	dot -Tps $*.dot -o $*.ps

clean:
	rm *.pdf

