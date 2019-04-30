WGET  ?= wget
RM     = rm -rf
EMACS ?= emacs
CASK ?= cask

PKG = conf-modes.el
LOAD_PATH ?=
LOAD_PATH += -L .
BATCH = $(EMACS) -Q -batch $(LOAD_PATH)

.PHONY: test clean distclean nvp
all: test
test:
	$(CASK) exec ert-runner

README.md: el2markdown.el ${PKG}
	$(BATCH) -l $< $(PKG) -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(WGET) \
  -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
