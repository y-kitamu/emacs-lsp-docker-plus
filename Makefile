EMACS ?= emacs
CASK ?= cask

LOAD_FILE = -l $(load-file)

PACKAGE_FILES = lsp-docker+.el
LOAD_PACKAGE_FILES = $(foreach load-file, $(PACKAGE_FILES), $(LOAD_FILE))

TEST_FILES = $(wildcard test/test*.el)
LOAD_TEST_FILES := $(foreach load-file, $(TEST_FILES), $(LOAD_FILE))

unix-ci: clean unix-build unix-compile unix-lint-test unix-unit-test

unix-build:
	$(CASK) install

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q -batch -L . -eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PACKAGE_FILES)

unix-lint-test:
	$(CASK) exec $(EMACS) -batch -L . $(LOAD_PACKAGE_FILES) \
		--eval "(setq byte-compile-error-on-warn t)" \
		--eval "(require 'package-lint)" -f package-lint-batch-and-exit

unix-unit-test:
	$(CASK) exec $(EMACS) -batch -L . $(LOAD_TEST_FILES) -f ert-run-tests-batch-and-exit

clean:
	rm -rf .cask *.elc */*.elc
