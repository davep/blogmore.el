all:
	emacs --batch -f batch-byte-compile blogmore.el

test:
	emacs --batch -l ert -l blogmore.el -l blogmore-tests.el -f ert-run-tests-batch-and-exit

### Makefile ends here
