# https://stackoverflow.com/a/18137056

PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name ".*" -printf '%f\n')

STOW_FLAGS := --verbose

$(PKGS):
	stow $(STOW_FLAGS) --stow $@

all: $(PKGS)

clean:
	stow $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) all clean test
