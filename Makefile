EMACS = emacs
EMACS_LOAD = $(EMACS) -Q --batch --load
FORTH = gforth-0.7.3

SRC = $(wildcard *.el) $(wildcard backend/*.el)

all: forth-mode.elc

forth-mode.elc: $(SRC)
	FORTH=$(FORTH) $(EMACS_LOAD) build.el

doc: forth-mode.info

%.info: %.texi
	makeinfo $<

check: forth-mode.elc
	FORTH=$(FORTH) $(EMACS) -Q --batch -L . -l test/tests.el \
	-f ert-run-tests-batch-and-exit

clean:
	rm -f autoloads.el *.elc backend/*.elc
