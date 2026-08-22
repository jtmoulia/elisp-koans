EMACS ?= emacs

# Load the user's package directory so `dash' is found, then this repo.
INIT := --eval "(progn (require 'package) (package-initialize))"
LOAD := -L . -L test

BATCH := $(EMACS) -Q --batch $(INIT) $(LOAD)

.PHONY: all test test-cheatsheet compile clean

all: compile test-cheatsheet test

## Run the solutions in cheatsheet/.  Every koan must pass; this is the only
## executable check that the koans themselves still work.
test-cheatsheet:
	$(BATCH) -l elisp-koans \
	  --eval '(kill-emacs (if (elisp-koans/run-tests-batch elisp-koans-cheatsheet-directory) 0 1))'

## Run the unit tests, including the koan/cheatsheet sync checks.
test:
	$(BATCH) -l elisp-koans \
	  -l test/test-elisp-koans.el -l test/test-cheatsheet.el \
	  --eval '(ert-run-tests-batch-and-exit)'

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn nil)' \
	  -f batch-byte-compile elisp-koans.el

clean:
	rm -f *.elc koans/*.elc cheatsheet/*.elc test/*.elc
