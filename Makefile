EMACS = emacs
EMACS_LOAD = $(EMACS) -Q --batch --load

all: forth-mode.elc

forth-mode.elc: $(wildcard *.el)
	$(EMACS_LOAD) build.el

doc: forth-mode.info

%.info: %.texi
	makeinfo $<

clean:
	rm -f autoloads.el *.elc
