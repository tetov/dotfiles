;;; editor-backup.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'org)

(defun tetov/org-file-has-any-tag-p (filename tags)
  "Check if any node in the org FILENAME has any of the TAGS."
  (with-current-buffer (find-file-noselect filename)
    (cl-some #'identity (org-map-entries
                         (lambda ()
                           (seq-intersection tags (org-get-tags)))))))

(setq tetov/editor-backup-dir (file-name-concat "editor-backups" "emacs" (system-name)))

(defun tetov/backup-each-save-filter (filename)
  "Determine whether to backup FILENAME."
  (let ((filename-ignore-patterns `(,tetov/editor-backup-dir
                                    "semantic.cache$"
                                    "\\.emacs-places$"
                                    "\\.?recentf$"
                                    ".newsrc\\(\\.eld\\)?"))
        (org-tag-ignore-list '("crypt")))
  (not (or (seq-some (lambda (pattern)
                       (string-match-p pattern filename))
                     filename-ignore-patterns)
           (when (string-match-p "\\.org$" filename)
             (tetov/org-file-has-any-tag-p filename
                                           org-tag-ignore-list))
           (bound-and-true-p mu4e-compose-mode)
           (bound-and-true-p git-commit-mode)))))
;; (progn
;;   (if should-ignore (message "backup-each-save ignoring %s" filename)
;;     (message "backup-each-save saving %s" filename))
;;   (not should-ignore))))

(defun tetov/editor-backup-setup-dir (topdir)
  (file-name-concat topdir tetov/editor-backup-dir))

(provide 'editor-backup)
