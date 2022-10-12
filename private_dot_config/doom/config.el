
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

(defun tetov/open-my-agenda-view () "" (interactive nil) (org-agenda nil "d"))
(defun tetov/org-capture-default () "Start default org-capture" (interactive nil) (org-capture nil "d"))

(map! :leader (:prefix-map ("m" . "mine")
                           (:desc "(A)genda view" "a" #'tetov/open-my-agenda-view)
                           (:desc "roam (b)uffer toggle" "b" #'org-roam-buffer-toggle)
                           (:desc "(e)-shell" "e" #'+eshell/toggle)
                           (:desc "roam (f)ind node" "f" #'org-roam-node-find)
                           (:desc "org (g)oto last captured" "g" #'org-capture-goto-last-stored)
                           (:desc "org (G)oto last refiled" "G" #'org-refile-goto-last-stored)
                           (:desc "(M)ail menu" "m" #'mu4e-headers-search-bookmark)
                           (:desc "Compose (M)ail" "M" #'+mu4e/compose)
                           (:desc "roam (r)efile" "r" #'org-roam-refile)
                           (:desc "org (R)efile" "R" #'org-refile)
                           (:desc "(v)-term" "v" #'+vterm/toggle)
                           (:desc "org capture default" "X" #'tetov/org-capture-default)))

;; open in new tab instead of same tab..
(setq browse-url-new-window-flag t)
(setq browse-url-firefox-new-window-is-tab t)

(setq doom-localleader-key ",")

;; vimify
(setq evil-respect-visual-line-mode t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t
      evil-move-cursor-back nil)

(evil-put-command-property 'evil-yank-line :motion 'evil-line)

(evil-global-set-key 'motion "Ö" 'evil-ex)
(evil-global-set-key 'motion "¤" 'evil-end-of-line)

;; splits
;; (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
;; (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
;; (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
;; (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)

;; paragraphs
(evil-global-set-key 'motion (kbd "<backspace>") 'evil-backward-paragraph)
(evil-global-set-key 'motion (kbd "RET") 'evil-forward-paragraph)

;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" 'kill-buffer-and-window)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)

;; break lines automatically
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; completion
(setq company-dabbrev-other-buffers 'all)

;; vertico don't delete whole dir on backspace
(defun my/vertico-directory-delete-char (&optional n)
  "Delete N chars before point."
  (interactive "p")
  (backward-delete-char n))

(after! vertico
  (define-key vertico-map (kbd "<backspace>") #'my/vertico-directory-delete-char))

(defun my/count-characters-subtree ()
  "Count characters in subtree.

   Useful for Vinnova project reports.

   Taken from https://stackoverflow.com/a/50958323"
  (interactive nil)
  (save-excursion
    (org-mark-subtree) ;mark the whole subtre
    (forward-line 1)   ;move past header
    (exchange-point-and-mark) ;swap point and mark (ends of region)
    (forward-line -1)  ;move backwards past the last line
    (let ((nchars (- (point) (mark))))
      (deactivate-mark) ;clear the region
      (message "%d" nchars))))

;;org
;; must be set before org loads!
(setq org-directory "~/src/org/")

(require 'bh)
(after! (org org-roam)
  ;; files
  (setq org-agenda-files (directory-files org-directory nil (rx ".org" eos)))
  (setq org-default-notes-file (concat org-directory "/refile.org"))
  (setq org-attach-id-dir (expand-file-name "~/Nextcloud/Apps/org-attach"))
  ;; general
  (setq org-startup-folded t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-startup-indented t)
  (setq org-insert-heading-respect-content t)
  ;; references
  (setq bibtex-completion-bibliography '("~/gdrive/zot.bib"))
  ;; refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  ;; capture templates
  ;; http://doc.norang.ca/org-mode.html#CaptureTemplates
  (setq org-capture-templates `(("d" "default" entry (file org-default-notes-file)
                                 "* TODO %?\n%U\n" :clock-in t :clock-resume t)
                                ("m" "Meeting" entry (file org-default-notes-file)
                                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                ("e" "Email" entry (file org-default-notes-file)
                                 "* TODO %:fromname: %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))"
                                 :clock-in t :clock-resume t)))

  ;; create id's for all captures
  (add-hook 'org-capture-mode-hook #'org-id-get-create)

  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out)

  ;; todo setup
  (setq org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "NEXT(n)" "|" "DONE(d!)")
                            (sequence "WAIT(w@/!)" "|" "CANC(c@/!)" "MEETING" "PHONE")))
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-use-fast-todo-selection t)
  (setq org-log-state-notes-into-drawer t)

  ;; clocks
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  (setq bh/organization-task-id "a5b03c9e-2390-4ebe-9282-fa901a564a17")

  (add-hook 'org-clock-out-hook 'bh/clock-out-maybe)
  ;;
  ;; refile tweaks
  ;; http://doc.norang.ca/org-mode.html#RefileSetup
  (setq org-refile-target-verify-function 'bh/verify-refile-target)

  ;; Tags with fast selection keys
  (setq org-tag-alist (quote ((:startgroup)
                              ("@work" . ?o)
                              ("@home" . ?H)
                              (:endgroup)
                              ("rp" . ?r)
                              )))

  ;; projects setup
  (add-to-list 'org-tags-exclude-from-inheritance "project")

  ;; archiving
  (setq org-archive-mark-done nil)

  ;; links
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; export
  (setq org-export-with-broken-links 'mark)

  ;; org-roam
  (setq org-roam-directory org-directory)
  (setq org-roam-db-location (concat org-roam-directory "/db/org-roam.db"))
  ;; roam template
  (setq org-roam-capture-templates
        '(("n" "note" plain "%?"
           :if-new (file+head "notes/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: note
:END:
#+title: ${title}
%U")
           :immediate-finish t
           :unnarrowed t)
          ("p" "(meta) project" plain  "%?"
           :if-new (file+head "${slug}.org"
                              ":PROPERTIES:
:CATEGORY: project
:END:
#+title: ${title}
%U")
           :immediate-finish t
           :unnarrowed t)
          ("w" "writing" plain  "%?"
           :if-new (file+head "writing/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: writing
:END:
#+title: ${title}
%U")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "refs/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: reference
:ROAM_REFS: %x
:END:
#+title: ${title}
%U")
           :immediate-finish t
           :unnarrowed t)
          ("b" "bibliography reference" plain "%?"
           :if-new (file+head "refs/${citekey}.org"
                              ":PROPERTIES:
:CATEGORY: reference
:END:
#+PROPERTY: type %^{entry-type}
#+FILETAGS: %^{keywords}
#+PROPERTY: authors %^{author}
#+title: ${title}\n")
           :unnarrowed t)
          ("o" "rp notes (Eat Flay Prowl)" plain "%?"
           :if-new (file+head "rp/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: rp
:END:
#+filetags: :dnd5e:eat-flay-prowl:
#+title: ${title}
%U")
           :immediate-finish t
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates '(("r" "ref" plain "%?" :target
                                          (file+head "refs/${slug}.org" ":PROPERTIES:
:CATEGORY: reference
:END:
#+title: ${title}
%U")
                                          :unnarrowed t))
        ))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links

;; agenda
(require 'sv-kalender)
(after! org-agenda
  (setq org-agenda-include-diary t)
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view

  ;; keep agenda view alive
  (setq org-agenda-sticky t)
  (setq org-agenda-span 7)
  (setq org-agenda-start-with-log-mode t)

  (setq org-agenda-clock-consistency-checks '(:max-duration "7:00"
                                              :min-duration 0
                                              :max-gap 5
                                              :gap-ok-around ("4:00" "13:00")))
  ;; agenda views
  (use-package! org-ql)
  (org-ql-defpred is-project-p () "" :body (and (todo "TODO")
                                                (descendants (todo))))
  (org-ql-defpred is-stuck-project-p () "" :body (and (is-project-p)
                                                      (not (descendants (todo "NEXT" "PROG")))))
  (setq org-agenda-custom-commands
        '(("d" "default"
           ((tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to refile")
                   (org-tags-match-list-sublevels nil)))
            (agenda "" nil)
            (org-ql-block '(and (todo "TODO")
                                (descendants (todo))
                                (not (descendants (todo "PROG" "NEXT" "WAIT"))))
                          ((org-ql-block-header "Stuck projects")))
            (todo "NEXT|PROG"
                  ((org-agenda-overriding-header "Next or in progress")
                   (org-agenda-sorting-strategy '(todo-state-up category-up))))
            (todo "TODO"
                  ((org-agenda-overriding-header "TODOs")
                   (org-agenda-skip-function 'bh/skip-projects-and-habits)
                   (org-agenda-sorting-strategy '(todo-state-down deadline-up scheduled-up))))

            (org-ql-block '(and (todo "TODO") (descendants (todo))) ((org-ql-block-header "All projects")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting"))))))))

;; backup

(defun backup-each-save-filter (filename)
  (let ((ignored-filenames
    	 '("^/tmp" "semantic.cache$" "\\.emacs-places$"
    	   "\\.?recentf$" ".newsrc\\(\\.eld\\)?"))
    	(matched-ignored-filename nil))
    (mapc
     (lambda (x)
       (when (string-match x filename)
    	 (setq matched-ignored-filename t)))
     ignored-filenames)
    (not matched-ignored-filename)))

(use-package! backup-each-save)
(setq backup-each-save-mirror-location (format "~/Nextcloud/Apps/editor-backups/emacs/%s" (system-name)) ;; put files under hostname
      backup-each-save-remote-files t
      backup-each-save-filter-function 'backup-each-save-filter)
(add-hook 'after-save-hook 'backup-each-save)

;; autosave on focus lost
;; https://emacs.stackexchange.com/a/60971
(add-function :after after-focus-change-function
              (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; pocket-reader
(after! pocket-reader
  (require 'org-pocket)
  (setq org-pocket-capture-file "~/src/org/refile.org"))

;; completion
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; highlight indentation
(setq highlight-indent-guides-method 'fill)

;; ansible
(add-hook 'ansible-hook #'lsp!)

;; python
;; use format-all, not lsp formatter
(setq poetry-tracking-strategy 'projectile)
(after! python-mode (setq poetry-tracking-strategy 'projectile))
(setq-hook! 'python-mode-hook +format-with-lsp nil)
(setq-hook! 'python-mode-hook poetry-tracking-strategy 'projectile)
;; spelling
;; based on https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-dictionary "sv_SE,en_GB")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "sv_SE,en_GB")
  (setq ispell-local-dictionary-alist
        '(("sv_SE,en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "sv_SE,en_GB") nil utf-8)))
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0)))

;; contacts
(setq vdirel-repository ( substitute-in-file-name "$XDG_DATA_HOME/vdirsyncer/contacts/Default"))

;; mail
(defun tetov/mu4e-refile-folder-function (msg)
  "Set the refile directory for message.

   Refile with this function means moving msg from INBOX to Archive. If the msg
   is not in INBOX leave it be (set refile dir to current dir it is in)."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (string-replace "/INBOX" "/Archive" maildir)))

(set-email-account! "fastmail"
                    '((mu4e-sent-folder       . "/fastmail/Sent")
                      (mu4e-drafts-folder     . "/fastmail/Drafts")
                      (mu4e-trash-folder      . "/fastmail/Trash")
                      (smtpmail-smtp-user     . "tetov@fastmail.com")
                      (+mu4e-personal-addresses . '("anton@tetov.se" "tetov@fastmail.com"))
                      (mu4e-compose-signature . "Best regards\nAnton Tetov Johansson"))
                    t)

(set-email-account! "lth"
                    '((mu4e-sent-folder        . "/lth/Sent Items")
                      (mu4e-drafts-folder      . "/lth/Drafts")
                      (mu4e-trash-folder       . "/lth/Deleted Items")
                      (smtpmail-smtp-user      . "anton_tetov.johansson@abm.lth.se")
                      (+mu4e-personal-addresses . '("anton_tetov.johansson@abm.lth.se"
                                                    "anton_tetov.johansson@control.lth.se"
                                                    "anton.johansson@abm.lth.se"
                                                    "anton.johansson@control.lth.se"))
                      (mu4e-compose-signature  . "Best regards,
Anton Tetov Johansson

Project assistant
Lund University - LTH
Department of Architecture and Built Environment
& Department of Automatic Control

Phone no: +46 70-363 56 67

https://abm.lth.se/
https://control.lth.se/"))
                    nil)

(after! mu4e
  ;; mail box updated using systemd timer, so mail command is set to true
  ;; mu4e still indexes again but that should be fine.
  (setq mu4e-get-mail-command "true")

  ;; disable org-msg
  (setq mu4e-compose--org-msg-toggle-next nil)

  (setq +org-capture-emails-file "refile.org")

  (setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               '( :name "allinboxes"
                  :key  ?i
                  :query "maildir:/lth/INBOX OR maildir:/fastmail/INBOX"))
  (setq message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote:")
  (setq mu4e-headers-skip-duplicates nil)
  (setq mu4e-change-filenames-when-moving t)

  ;; ask for context when new message doesn't match context (i.e. new message)
  (setq mu4e-compose-context-policy 'ask)

  ;; refile func
  (setq mu4e-refile-folder 'tetov/mu4e-refile-folder-function)

  ;; (setq mu4e-split-view 'vertical)
  ;; (setq mu4e-headers-visible-columns 80)
  (setq mu4e-headers-fields
        '((:account-stripe . 1)
          (:human-date . 12)
          (:flags . 6)
          (:from-or-to . 25)
          (:maildir . 20)
          (:subject)))

  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (map! :localleader :map 'mu4e-view-mode-map :desc "Mark thread" "t" #'mu4e-view-mark-thread)
  (map! :localleader :map 'mu4e-headers-mode-map :desc "Mark thread" "t" #'mu4e-headers-mark-thread))

;; don't autosave to try to reduce number of drafts synced.
;; TODO: Check if it still saves to ~/editor-backups
(add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))

(run-at-time "5 sec" nil (lambda ()
                           (let ((current-prefix-arg '(4)))
                             (call-interactively 'mu4e)
                             (message nil))))
;; term
(after! vterm
  (set-popup-rule! "*doom:vterm-popup:" :size 0.35 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(after! eshell
  (set-popup-rule! "*doom:eshell-popup:" :size 0.35 :vslot -4 :select t :quit nil :ttl 0 :side 'right))

(set-eshell-alias!
 ;; from  doomemacs/modules/term/eshell/config.el
 "q"  "exit"           ; built-in
 "f"  "find-file $1"
 "ff" "find-file-other-window $1"
 "d"  "dired $1"
 "bd" "eshell-up $1"
 "rg" "rg --color=always $*"
 "l"  "ls -lh $*"
 "ll" "ls -lah $*"
 "git" "git --no-pager $*"
 "gg" "magit-status"
 "cdp" "cd-to-project"
 "clear" "clear-scrollback" ; more sensible than default

 ;; mine (adapted from zsh config)
 ".." "cd .."

 "_" "sudo $*"

 "r" "rolldice -s $*"

 "g" "git $*"
 "ga" "g add $*"
 "gb" "g branch $*"
 "gc" "g commit -v $*"
 "gcmsg" "gc -m \"$*\""
 "gcd" "(if (doom-project-root) (eshell/cd-to-project) (eshell/echo \"Not in project directory.\"))"
 "gco" "g checkout $*"
 "gd" "g diff $*"
 "gf" "g fetch $*"
 "gl" "g pull $*"
 "gp" "g push $*"
 "gr" "g push $*"
 "gst" "g status $*"

 "cm" "chezmoi $*)"

 "cmcd" "eshell/cd ${chezmoi source-path}")

;;projectile
(setq projectile-auto-discover t
      projectile-project-search-path '(("~/src" . 1)))  ;; number means search depth

;; java
(setq lsp-java-configuration-runtimes '[ ;;(:name "JavaSE-11" :path "/usr/lib/jvm/java-11-openjdk/")
                                        (:name "JavaSE-17" :path "/usr/lib/jvm/java-17-openjdk/" :default t)])

;; chezmoi
(use-package! chezmoi)
(after! chezmoi
  (require 'chezmoi-company))
(add-hook 'chezmoi-mode-hook #'(lambda () (if chezmoi-mode
                                              (add-to-list 'company-backends 'chezmoi-company-backend)
                                            (setq company-backends (delete 'chezmoi-company-backend company-backends)))))
(defun chezmoi--evil-insert-state-enter ()
  "Run after evil-insert-state-entry."
  (chezmoi-template-buffer-display nil (point))
  (remove-hook 'after-change-functions #'chezmoi-template--after-change 1))

(defun chezmoi--evil-insert-state-exit ()
  "Run after evil-insert-state-exit."
  (chezmoi-template-buffer-display nil)
  (chezmoi-template-buffer-display t)
  (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1))

(defun chezmoi-evil ()
  (if chezmoi-mode
      (progn
        (add-hook 'evil-insert-state-entry-hook #'chezmoi--evil-insert-state-enter nil 1)
        (add-hook 'evil-insert-state-exit-hook #'chezmoi--evil-insert-state-exit nil 1))
    (progn
      (remove-hook 'evil-insert-state-entry-hook #'chezmoi--evil-insert-state-enter 1)
      (remove-hook 'evil-insert-state-exit-hook #'chezmoi--evil-insert-state-exit 1))))
(add-hook 'chezmoi-mode-hook #'chezmoi-evil)

(map! :leader (:prefix-map ("d" . "chezmoi dotfiles")
                           (:desc "chezmoi apply" "a" #'chezmoi-write)
                           (:desc "chezmoi apply all" "A" #'chezmoi-write-files)
                           (:desc "chezmoi magit status" "s" #'chezmoi-magit-status)
                           (:desc "chezmoi diff" "d" #'chezmoi-diff)
                           (:desc "chezmoi ediff" "e" #'chezmoi-ediff)
                           (:desc "chezmoi find" "f" #'chezmoi-find)
                           (:desc "chezmoi open other file" "o" #'chezmoi-open-other)
                           (:desc "chezmoi template buffer display" "t" #'chezmoi-template-buffer-display)
                           (:desc "chezmoi toggle mode" "c" #'chezmoi-mode)))

;; elfeed (RSS)
(after! (elfeed elfeed-protocol)
  (setq elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (setq elfeed-feeds '(("owncloud+https://tetov@cloud.tetov.se"
                        :password (shell-command-to-string "echo -n `secret-tool lookup org privat provider nextcloud service rss user tetov`"))))
  (setq elfeed-protocol-enabled-protocols '(owncloud))
  (elfeed-protocol-enable))

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
