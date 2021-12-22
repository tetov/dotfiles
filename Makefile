.DEFAULT_GOAL := default

PATH_ADDITION := PATH=$$HOME/bin:$$HOME/.local/bin:$$PATH

# STOW PACKAGES
PKGS := $(shell find . -maxdepth 1 -type d ! -name "deps" ! -name "bin" ! -name ".*" -printf '%f\n')
STOW_FLAGS := --verbose --no-folding

STOW := $(PATH_ADDITION) stow

$(PKGS): update_plugins
	@$(STOW) $(STOW_FLAGS) --stow $@

$(PKGS): update_plugins

# UPDATE PLUGIN MANAGERS notes: restart zsh, tmux and vim to
# make sure everything is resourced
SUBMODULE_PLUGIN_MANAGERS := tmux/.tmux/plugins/tpm zsh/.zcomet/bin
VIM_PLUG_LOCATIONS := vim/.vim/autoload/plug.vim nvim/.local/share/nvim/site/autoload/plug.vim

$(SUBMODULE_PLUGIN_MANAGERS):
	git submodule update --remote $@

$(VIM_PLUG_LOCATIONS):
	curl -Lo $@ --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

update_plugin_managers: $(SUBMODULE_PLUGIN_MANAGERS) $(VIM_PLUG_LOCATIONS)
	git add $^
	git commit -m "updated plugin managers" || echo Nothing to commit

# UPDATE PLUGINS
PLUGIN_PATHS := '.zcomet/repos' '.tmux/plugins' '.vim/plugged'
VCS:= $(PATH_ADDITION) vcs

$(PLUGIN_PATHS): install_plugins
	$(VCS) pull $$HOME/$@

install_plugins:
	zsh -c "source zsh/.zshrc" # enough to install zcomet
	tmux/.tmux/plugins/tpm/bin/install_plugins
	vim +PlugInstall +qall

# note: tpm/bin/update_plugins only pulls
# note: vim-plug's :PlugUpdate only pulls
update_plugins: $(PLUGIN_PATHS) 
	# prob some compinit stuff + plugins that are not repos
	zsh -c "source zsh/.zcomet/bin/zcomet.zsh && zcomet self-update"  

# GENERAL
default: stow update_plugins

all: stow update_plugins update_plugin_managers

clean:
	$(STOW) $(STOW_FLAGS) --delete $(PKGS)

test:

.PHONY: $(PKGS) $(PLUGIN_PATHS) $(SUBMODULE_PLUGIN_MANAGERS) $(VIM_PLUG_LOCATIONS) all clean test default update_plugins update_plugin_managers stow install_plugins
