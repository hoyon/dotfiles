EMACS ?= emacs

# org-test.el is excluded: org.el needs a themed interactive session (ef-themes)
# and cannot be loaded under `emacs -Q --batch'. Run it with M-x ert instead.
TESTS := $(filter-out org-test.el,$(wildcard *-test.el))

# Stubbing a primitive (yes-or-no-p) makes Emacs native-compile a trampoline
# for it, which fails in batch; tests need no compilation to run.
EMACSFLAGS := -Q --batch -L . \
  --eval '(setq native-comp-enable-subr-trampolines nil native-comp-jit-compilation nil)' \
  -l ert

.PHONY: test
test:
	@fail=0; \
	for f in $(TESTS); do \
	  printf '\n=== %s\n' "$$f"; \
	  $(EMACS) $(EMACSFLAGS) -l "$$f" -f ert-run-tests-batch-and-exit || fail=1; \
	done; \
	exit $$fail

# make one T=workspaces-run-test.el
.PHONY: one
one:
	@$(EMACS) $(EMACSFLAGS) -l "$(T)" -f ert-run-tests-batch-and-exit
