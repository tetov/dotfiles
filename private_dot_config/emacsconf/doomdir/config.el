;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Anton Tetov Johansson"
      user-mail-address "anton@tetov.se")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/src/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-load-path! "../lisp")

(setq doom-localleader-key ",")

;; vimify
(setq evil-respect-visual-line-mode t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t)
(evil-put-command-property 'evil-yank-line :motion 'evil-line)

(evil-global-set-key 'motion "Ö" 'evil-ex)
(evil-global-set-key 'motion "¤" 'evil-end-of-line)

;; splits
(evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
(evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
(evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
(evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)

;; paragraphs
(evil-global-set-key 'motion (kbd "<backspace>") 'evil-backward-paragraph)
(evil-global-set-key 'motion (kbd "RET") 'evil-forward-paragraph)

;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" 'kill-buffer-and-window)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)

;; break lines automatically
(setq-default fill-column 80)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

;; org
(after! org
  (setq org-agenda-files (directory-files-recursively org-directory "^[[:alnum:]].*\\.org$")
        org-default-notes-file (concat org-directory "/refile.org")
        org-todo-keywords '((sequence "TODO" "PROG" "NEXT" "WAIT" "|" "DONE" "CANC"))
        org-startup-folded t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-refile-targets '((org-agenda-files :maxlevel . 5))
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-startup-indented t
        org-insert-heading-respect-content t

        ;; template
        org-capture-templates `(
                                ("t" "Todo" entry (file+headline "" "Tasks")
                                 "** TODO %^{Task Description}\nSCHEDULED: %t\n%U"))))

;; org-roam
(after! org-roam

  (setq org-roam-directory (concat org-directory "/roam")
        org-roam-db-location (concat org-roam-directory "/db/org-roam.db")

        ;; roam template
        org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "reference/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "rp notes (Eat Flay Prowl)" plain "%?"
           :if-new (file+head "rp/${slug}.org"
                              "#+FILETAGS: :dnd5e:eat-flay-prowl:\n#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t))))

(add-hook! 'org-mode-hook
  (auto-fill-mode 1)
  (set-fill-column 80))

(require 'sv-kalender)
(after! org-agenda
  (setq org-agenda-include-diary t))

;; backup
(use-package! backup-each-save)
(setq backup-each-save-mirror-location ( format "~/editor-backups/%s/emacs" (system-name))
      backup-each-save-remote-files t)
(add-hook 'after-save-hook 'backup-each-save)

;; autosave on focus lost
;; https://emacs.stackexchange.com/a/60971
(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; pocket-reader
(after! pocket-reader
  (require 'org-pocket)
  (setq org-pocket-capture-file "~/src/org/refile.org"))

;; completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Ansible
(add-hook 'ansible-hook #'lsp!)

;; https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
(after! smartparens
  (defun zz/goto-match-paren (arg)
    "Go to the matching paren/bracket, otherwise (or if ARG is not
    nil) insert %.  vi style of % jumping to matching brace."
    (interactive "p")
    (if (not (memq last-command '(set-mark
                                  cua-set-mark
                                  zz/goto-match-paren
                                  down-list
                                  up-list
                                  end-of-defun
                                  beginning-of-defun
                                  backward-sexp
                                  forward-sexp
                                  backward-up-list
                                  forward-paragraph
                                  backward-paragraph
                                  end-of-buffer
                                  beginning-of-buffer
                                  backward-word
                                  forward-word
                                  mwheel-scroll
                                  backward-word
                                  forward-word
                                  mouse-start-secondary
                                  mouse-yank-secondary
                                  mouse-secondary-save-then-kill
                                  move-end-of-line
                                  move-beginning-of-line
                                  backward-char
                                  forward-char
                                  scroll-up
                                  scroll-down
                                  scroll-left
                                  scroll-right
                                  mouse-set-point
                                  next-buffer
                                  previous-buffer
                                  previous-line
                                  next-line
                                  back-to-indentation
                                  doom/backward-to-bol-or-indent
                                  doom/forward-to-last-non-comment-or-eol
                                  )))
        (self-insert-command (or arg 1))
      (cond ((looking-at "\\s\(") (sp-forward-sexp) (backward-char 1))
            ((looking-at "\\s\)") (forward-char 1) (sp-backward-sexp))
            (t (self-insert-command (or arg 1))))))
  (map! "%" 'zz/goto-match-paren))
