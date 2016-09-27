all:
	emacs -q --no-site-file --batch --load build.el

clean:
	rm -f autoloads.el *.elc
