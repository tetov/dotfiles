.DEFAULT_GOAL := all

PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name ".*" -printf '%f\n')

STOW_FLAGS := --verbose --no-folding

PLUGIN_PATHS := '.zcomet/repos' '.tmux/plugins' '.vim/plugged'

PATH_ADDITION := 'PATH=$$HOME/bin:$$HOME/.local/bin:$$PATH'

STOW := $(PATH_ADDITION) stow
VCS:= $(PATH_ADDITION) vcs

$(PKGS): plugin_managers
	@$(STOW) $(STOW_FLAGS) --stow $@

plugin_managers:
	$(VCS) import < plugin_managers.repos

$(PLUGIN_PATHS):
	$(VCS) pull $$HOME/$@

update_plugins: $(PLUGIN_PATHS)

stow: $(PKGS)

all: stow update_plugins

clean:
	$(STOW) $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) $(PLUGIN_PATHS) all clean test plugin_managers stow
