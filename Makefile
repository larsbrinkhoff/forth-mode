EMACS = emacs
EMACS_LOAD = $(EMACS) -Q --batch --load
FORTH = gforth

STATUS := $(shell git status && echo $$?)
OUTSIDE_GIT_TREE := $(lastword $(STATUS))

ifeq ($(OUTSIDE_GIT_TREE),0)
VERSION := $(shell git show --pretty=format:"%cd.%t.%h" --date=short)
else
VERSION := $(shell date +%Y-%m-%d)
endif

$(info version is [${VERSION}])

$(shell echo "(defvar forth-mode-version \"$(VERSION)\")\n(provide 'forth-mode-version)\n" > forth-mode-version.el)

SRC = $(wildcard *.el) $(wildcard backend/*.el)

all: forth-mode.elc

forth-mode.elc: $(SRC)
	FORTH=$(FORTH) $(EMACS_LOAD) build.el

doc: forth-mode.info

%.info: %.texi
	makeinfo $<

clean:
	rm -f autoloads.el *.elc backend/*.elc
