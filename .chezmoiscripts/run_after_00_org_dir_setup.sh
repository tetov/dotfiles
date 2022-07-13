#!/usr/bin/sh

ORG_DIR="$HOME/src/org"

if [ -d "$ORG_DIR/.git" ] ; then
    cd "$ORG_DIR" || exit 1
    git config --local commit.gpgsign false
    git config --local core.sshCommand \
        "ssh -o 'IdentitiesOnly=yes' -i ~/.ssh/org_repo_rsa"
    pre-commit install

    git config --local --bool branch.main.sync true
    git config --local --bool branch.main.syncNewFiles true
fi
