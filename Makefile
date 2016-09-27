all:
	emacs -Q --batch --load build.el

clean:
	rm -f autoloads.el *.elc
