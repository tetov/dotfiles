#!/bin/sh

dconf dump / > $(chezmoi source-path)/dconf.ini
