EMACS = emacs
EMACS_LOAD = $(EMACS) -Q --batch --load
FORTH = gforth

SRC = $(wildcard *.el) $(wildcard backend/*.el)

all: forth-mode.elc

forth-mode.elc: $(SRC)
	FORTH=$(FORTH) $(EMACS_LOAD) build.el

doc: forth-mode.info

%.info: %.texi
	makeinfo $<

clean:
	rm -f autoloads.el *.elc backend/*.elc
