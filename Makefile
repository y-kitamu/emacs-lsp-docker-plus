EMACS ?= emacs
CASK ?= cask

PACKAGE_FILES = lsp-docker+.el

TEST_FILES = $(wildcard test/test*.el)
LOAD_FILE = -l $(test-file)
LOAD_TEST_FILES := $(foreach test-file, $(TEST_FILES), $(LOAD_FILE))

unix-ci: clean unix-build unix-compile unix-test

unix-build:
	$(CASK) install

unix-compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q -batch -L . -eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile $(PACKAGE_FILES)

unix-test:
	$(CASK) exec $(EMACS) -batch -L . $(LOAD_TEST_FILES) -f ert-run-tests-batch-and-exit

clean:
	rm -rf .cask *.elc */*.elc
