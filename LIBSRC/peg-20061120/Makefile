check-silent:
	@rm -f test.log
	@for testfile in `cat test/TESTS`; do \
		gosh -I. test/$$testfile >> test.log; \
	done

check:
	@rm -f test.log
	@for testfile in `cat test/TESTS`; do \
		gosh -I. test/$$testfile | tee test.log; \
	done

test: check

bench:
	gosh -I. benchmark.scm

clean:
	@rm -f test.log

.PHONY: test check check-silent
