#!/bin/python
import grp
import os
import shutil
import sys
from pathlib import Path

TRAMPOLINE_DIR = Path(os.environ["XDG_RUNTIME_DIR"] + "/trampoline/dir")

def exit_if_not_regler_managed():
    try:
        regler_gid = grp.getgrnam("regler")[2]
        if os.getgid() != regler_gid:
            print("Not in regler group")
            sys.exit(0)
    except KeyError:
        print("regler group doesn't exist")
        sys.exit(0)


if __name__ == "__main__":
    exit_if_not_regler_managed()

    symlink_dirs = [
        ".local/share/vim",
        ".local/share/nvim",
        ".local/share/tmux",
        ".local/share/zsh",
        ".local/share/flatpak",
    ]

    symlink_link_names = [Path.home().joinpath(d) for d in symlink_dirs]
    symlink_targets = [TRAMPOLINE_DIR.joinpath(d) for d in symlink_dirs]

    for dir_ in symlink_link_names:
        dir_.parent.mkdir(parents=True, exist_ok=True)

    for dir_ in symlink_targets:
        dir_.mkdir(parents=True, exist_ok=True)

    for target, link_name in zip(symlink_targets, symlink_link_names):
        if link_name.exists():
            if link_name.is_dir():
                print(f"Moving {link_name} to {target}")
                shutil.copytree(link_name, target, dirs_exist_ok=True)
                shutil.rmtree(link_name)

            elif link_name.is_symlink():
                link_name.unlink()

        print(f"Creating symlink from {link_name} to {target}")
        link_name.symlink_to(target, target_is_directory=True)
