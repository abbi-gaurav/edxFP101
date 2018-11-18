.PHONY: all build lint test \
	print-release-version check-tool-stack

all: build
test: test-unit

VERSION=$(shell awk '/version\:\s+([0-9\.]+)/{print $$2}' haskell-restish-todo.cabal)

check-tool-stack:
#ifeq (, $(STACK))
#	$(error "`stack` doesn't seem to be installed (https://haskellstack.org)")
#endif

print-release-version:
	@echo "${VERSION}"

lint:
	hlint src/ app/ test/

test-unit:
	stack test :unit

build: check-tool-stack
	stack build
