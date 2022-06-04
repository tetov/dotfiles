#!/usr/bin/env zsh

cd $(chezmoi source-path)

git config --local commit.gpgsign false
git config --local core.sshCommand \
    "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/dotfiles_repo_rsa"
(( $+commands[pre-commit] )) && pre-commit install

[[ -d ~/src/org ]] || git clone git@github.com:tetov/org ~/src/org

cd ~/src/org

git config --local commit.gpgsign false
git config --local core.sshCommand \
    "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/org_repo_rsa"

exit 0
