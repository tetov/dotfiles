.DEFAULT_GOAL := all

PATH_ADDITION := PATH=$$HOME/bin:$$HOME/.local/bin:$$PATH

# STOW PACKAGES
PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name ".*" -printf '%f\n')
STOW_FLAGS := --verbose --no-folding

STOW := $(PATH_ADDITION) stow

$(PKGS): update_plugins
	@$(STOW) $(STOW_FLAGS) --stow $@

$(PKGS): update_plugins

# UPDATE PLUGIN MANAGERS
SUBMODULE_PLUGIN_MANAGERS = tmux/.tmux/plugins/tpm zsh/.zcomet/bin
ALL_PLUGIN_MANAGERS = $(SUBMODULE_PLUGIN_MANAGERS) vim/.vim/autoload/plug.vim

$(SUBMODULE_PLUGIN_MANAGERS):
	git submodule update --remote $@

vim/.vim/autoload/plug.vim:
	curl -fLo $@ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

update_plugin_managers: $(ALL_PLUGIN_MANAGERS)
	git add $^
	git commit -m "updated plugin managers"

# UPDATE PLUGINS
PLUGIN_PATHS := '.zcomet/repos' '.tmux/plugins' '.vim/plugged'
VCS:= $(PATH_ADDITION) vcs

$(PLUGIN_PATHS):
	$(VCS) pull $$HOME/$@

update_plugins: $(PLUGIN_PATHS)

# GENERAL

all: stow update_plugins

clean:
	$(STOW) $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) $(PLUGIN_PATHS) $(SUBMODULE_PLUGIN_MANAGERS) all clean test update_plugins update_plugin_managers stow
