wget  ?= wget
RM     = rm -rf
emacs ?= emacs

.PHONY: test clean distclean nvp
all: test
test:
	$(emacs) -Q -batch --eval '(progn (push "." load-path))' \
	-L . -l ert -l test/conf-tests.el                        \
	-f ert-run-tests-batch-and-exit

README.md: el2markdown.el conf-modes.el
	$(emacs) -batch -l $< conf-modes.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
