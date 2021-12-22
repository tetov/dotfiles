.DEFAULT_GOAL := default

PATH_ADDITION := PATH=$$HOME/bin:$$HOME/.local/bin:$$PATH

# STOW PACKAGES
PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name "submodule_plugin_managers" ! -name ".*" -printf '%f\n')
STOW_FLAGS := --verbose --no-folding

STOW := $(PATH_ADDITION) stow

$(PKGS): 
	@$(STOW) --verbose --no-folding --stow $@

submodule_plugin_managers: $(PKGS)
	$(STOW) --verbose --stow $@
	$(STOW) --verbose --restow --stow $@

stow: $(PKGS) 

# UPDATE PLUGIN MANAGERS notes: restart zsh, tmux and vim to
# make sure everything is resourced
SUBMODULE_PLUGIN_MANAGERS := submodule_plugin_managers/.tmux/plugins/tpm submodule_plugin_managers/.zcomet/bin
VIM_PLUG_LOCATIONS := vim/.vim/autoload/plug.vim nvim/.local/share/nvim/site/autoload/plug.vim

$(SUBMODULE_PLUGIN_MANAGERS):
	git submodule update --remote $@

$(VIM_PLUG_LOCATIONS):
	curl -Lo $@ --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

update_plugin_managers: $(SUBMODULE_PLUGIN_MANAGERS) $(VIM_PLUG_LOCATIONS)
	git diff --cached --quiet --exit-code  # should stop make if there's staged files
	git add $^
	git commit \
		-m "updated plugin managers" \
		--no-verify \
		|| echo Nothing to commit

# UPDATE PLUGINS
PLUGIN_PATHS := '.zcomet/repos' '.tmux/plugins' '.vim/plugged'
VCS:= $(PATH_ADDITION) vcs

$(PLUGIN_PATHS): install_plugins
	$(VCS) pull $$HOME/$@

install_plugins:
	zsh -c "source zsh/.zshrc" # enough to install zcomet
	submodule_plugin_managers/.tmux/plugins/tpm/bin/install_plugins
	vim +PlugInstall +qall

# note: tpm/bin/update_plugins only pulls
# note: vim-plug's :PlugUpdate only pulls
update_plugins: $(PLUGIN_PATHS) 
	# prob some compinit stuff + plugins that are not repos
	zsh -c \
		"source ./submodule_plugin_managers/.zcomet/bin/zcomet.zsh && zcomet update"  

# GENERAL
install_pre_commit_hooks:
	pre-commit install

default: stow update_plugins install_pre_commit_hooks 

all: update_plugin_managers default

clean:
	$(STOW) $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) $(PLUGIN_PATHS) $(SUBMODULE_PLUGIN_MANAGERS) $(VIM_PLUG_LOCATIONS) all clean test default submodule_plugin_managers update_plugins update_plugin_managers stow install_plugins
