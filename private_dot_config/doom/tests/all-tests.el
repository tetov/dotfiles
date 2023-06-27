;;; tests/all-tests.el -*- lexical-binding: t; -*-

;; If the directory happens to have both compiled and uncompiled
;; version, prefer to use the newer (typically the uncompiled) version.
(setq load-prefer-newer t)

(add-to-list 'load-path (file-name-directory (or load-file-name
                                                 (buffer-file-name))))
(require 'teditor-backup)

(ert-run-tests-batch-and-exit)
