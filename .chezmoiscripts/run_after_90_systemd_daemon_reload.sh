#!/bin/sh

systemctl --user daemon-reload
systemctl --user daemon-reexec
