EMACS ?= emacs
EMACS_LOAD = $(EMACS) -Q --batch --load

all:
	$(EMACS_LOAD) build.el

clean:
	rm -f autoloads.el *.elc
