#!/usr/bin/sh

cd $(chezmoi source-path)

git config --local commit.gpgsign false
git config --local core.sshCommand \
    "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/dotfiles_repo_rsa"
pre-commit install
