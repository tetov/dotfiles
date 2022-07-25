#!/usr/bin/sh

cd $(chezmoi source-path)

git config --local commit.gpgsign false
git config --local core.sshCommand \
    "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/dotfiles_repo_rsa"
git config --local filter.sylpheed_remove_passwords.smudge \
    "sed -Ee 's/^password=.*$/password=/'"

pre-commit install
