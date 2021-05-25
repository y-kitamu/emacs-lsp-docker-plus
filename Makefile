EMACS ?= emacs
CASK ?= cask

LOAD_FILE = -l $(load-file)

PACKAGE_FILES = lsp-docker+.el
LOAD_PACKAGE_FILES = $(foreach load-file, $(PACKAGE_FILES), $(LOAD_FILE))

TEST_FILES = $(wildcard test/test*.el)
LOAD_TEST_FILES := $(foreach load-file, $(TEST_FILES), $(LOAD_FILE))

unix-ci: clean unix-build unix-compile unix-test

unix-build:
	$(CASK) install

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q -batch -L . -eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PACKAGE_FILES)

unix-test:
	$(CASK) exec $(EMACS) -batch -L . $(LOAD_PACKAGE_FILES) \
		--eval "(require 'package-lint)" -f package-lint-batch-and-exit
	$(CASK) exec $(EMACS) -batch -L . $(LOAD_TEST_FILES) -f ert-run-tests-batch-and-exit

clean:
	rm -rf .cask *.elc */*.elc
