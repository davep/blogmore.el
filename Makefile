EMACS_BATCH = HOME=/tmp/emacs-sandbox emacs -Q --batch -l dev-environment.el

.PHONY: all
all:
	@echo "Byte-compiling blogmore.el..."
	$(EMACS_BATCH) -f batch-byte-compile blogmore.el

.PHONY: test
test:
	@echo "Running test suite..."
	$(EMACS_BATCH) -l blogmore.el -l blogmore-tests.el -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	@echo "Cleaning elc files..."
	rm -f *.elc

.PHONY: veryclean
veryclean: clean
	@echo "Cleaning dependencies..."
	rm -rf .packages

### Makefile ends here
