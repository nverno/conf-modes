WGET  ?= wget
RM    =  rm -rf
EMACS ?= emacs
CASK  ?= cask

PKG       =  conf-modes.el
LOAD_PATH ?= 
LOAD_PATH += -L .
BATCH     =  $(EMACS) -Q -batch $(LOAD_PATH)

.PHONY: test clean distclean nvp inputrc
all: test
test:
	$(CASK) exec ert-runner

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
