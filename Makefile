.DEFAULT_GOAL := all

PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name ".*" -printf '%f\n')

STOW_FLAGS := --verbose --no-folding

PLUGIN_PATHS := '.zcomet/repos' '.tmux/plugins' '.vim/plugged'

$(PKGS): plugin_managers
	@stow $(STOW_FLAGS) --stow $@

plugin_managers:
	vcs import < plugin_managers.repos

$(PLUGIN_PATHS):
	vcs pull $$HOME/$@

update_plugins: $(PLUGIN_PATHS)

stow: $(PKGS)

all: stow update_plugins

clean:
	stow $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) $(PLUGIN_PATHS) all clean test plugin_managers stow
