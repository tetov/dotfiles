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
(require 'cl-lib) ; For cl-letf

;; own files
(require 'tetov)

;;;; directory setup
(setq tetov/win-user-dir "/mnt/c/Users/tetov")
(setq tetov/nextcloud-dir (expand-file-name "~/Nextcloud"))
(setq tetov/nextcloud-apps-dir (file-name-concat tetov/nextcloud-dir "Apps"))

;;;; funcs outside of after! blocks
(defun tetov/open-my-agenda-view () "" (interactive nil) (org-agenda nil "d"))
(defun tetov/org-capture-default () "Start default org-capture" (interactive nil) (org-capture nil "d"))

(defun tetov/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Purple") ; org-agenda source
    (cfw:cal-create-source "Orange") ; diary source
    (cfw:ical-create-source "fastmail" (1password-get-field "fpbrakjsxo5ry3ubnhayv7xzka" "main_calendar_ics") "IndianRed"))))

;;;; prefix map (SPC-\)
(map! :leader (:prefix-map ("l" . "lmine")
                           (:desc "(A)genda view" "a" #'tetov/open-my-agenda-view)
                           (:desc "roam (b)uffer toggle" "b" #'org-roam-buffer-toggle)
                           (:desc "(e)-shell" "e" #'+eshell/toggle)
                           (:desc "roam (f)ind node" "f" #'org-roam-node-find)
                           (:desc "org (g)oto last captured" "g" #'org-capture-goto-last-stored)
                           (:desc "org (G)oto last refiled" "G" #'org-refile-goto-last-stored)
                           (:desc "(M)ail menu" "m" #'mu4e-search-bookmark)
                           (:desc "Compose (M)ail" "M" #'+mu4e/compose)
                           (:desc "roam (r)efile" "r" #'org-roam-refile)
                           (:desc "org (R)efile" "R" #'org-refile)
                           (:desc "org-(q)l views" "q" #'org-ql-view)
                           (:desc "(v)-term" "v" #'+vterm/toggle)
                           (:desc "org capture default" "\\" #'tetov/org-capture-default)))

;;;; browse-url
;; configure firefox
;; open in new tab instead of same tab..
(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)

;; configure for wsl
;; https://hungyi.net/posts/browse-emacs-urls-wsl/
(when (tetov-is-wsl-p)
  (setq browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
        browse-url-generic-args     '("/c" "start")))

;; termux / android
(when (tetov-is-android-p)
  (setq browse-url-generic-program "termux-open-url"))

;; set browse-url-browser-function
(setq browse-url-browser-function (if (or (tetov-is-wsl-p) (tetov-is-android-p))
                                      'browse-url-generic
                                    'browse-url-firefox))

(setq doom-localleader-key ",")
;; highlight indentation
(setq highlight-indent-guides-method 'fill)

;;;; vimify
(setq evil-respect-visual-line-mode t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-want-Y-yank-to-eol t
      evil-want-fine-undo t
      evil-move-cursor-back nil
      evil-snipe-override-evil-repeat-keys nil)

(after! evil
  (evil-put-command-property 'evil-yank-line :motion 'evil-line)
  (evil-global-set-key 'normal (kbd "j") 'evil-next-visual-line)
  (evil-global-set-key 'normal (kbd "k") 'evil-previous-visual-line)

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

  ;; check the mappings by inspecting evil-ex-commands (SPC h v)

  ;; Need to type out :quit to close frame
  ;; (evil-ex-define-cmd "quit" ' suspend-frame)
  ;; :q should kill the current buffer rather than quitting emacs entirely
  ;; (evil-ex-define-cmd "q" 'kill-buffer-and-window)
  ;; (evil-ex-define-cmd "quitall" 'kill-some-buffers)
  ;; (evil-ex-define-cmd "wqall" ' evil-save-and-close)

  (evil-ex-define-cmd "mu[4e]" 'mu4e-search-bookmark)

  (evil-global-set-key 'motion (kbd "[e") 'flycheck-previous-error))


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

;;;; fill
;; break lines automatically
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;;; completion
(after! (:or vertico orderless)
  (setq orderless-smart-case t
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t))

;;; org

;;;;; org file locations (before after!)
(setq org-directory (file-name-concat tetov/nextcloud-apps-dir "org")
      org-agenda-files (list org-directory (file-name-concat org-directory "daily"))
      org-default-notes-file (file-name-concat org-directory "refile.org")
      +org-capture-notes-file org-default-notes-file
      org-attach-id-dir (file-name-concat tetov/nextcloud-apps-dir "org-attach")
      org-id-locations-file (file-name-concat doom-cache-dir ".org-id-locations")
;;;;;; roam files
      org-roam-directory org-directory)

;;;; paths for references
(let* ((bib-paths (list (file-name-concat org-directory "zotero.bib")))
       (bib-notes-subdir "refs")
       (bib-note-path (file-name-concat org-directory bib-notes-subdir "/"))
       (bib-note-paths (list bib-note-path)))
  (setq! bibtex-completion-bibliography bib-paths
         bibtex-completion-notes-path bib-note-path
         citar-bibliography bib-paths
         citar-notes-paths bib-note-paths
         citar-org-roam-subdir bib-notes-subdir))

(after! org
  (require 'org-element)
  (require 'bh)
  (require 'org-crypt)
  (require 'org-clock-convenience)
  (require 'sv-kalender)

  (defun tetov/remove-org-id-links-export-hook (backend)
    "Remove 'id' type links from the current buffer before export to BACKEND."
    (tetov/remove-org-id-links-in-buffer))

  ;; TODO fix
  (defun tetov/remove-id-links-in-buffer ()
    "Remove 'id' type links from the current buffer."
    (interactive)
    (let ((parsed-org (org-element-parse-buffer)))
      (org-element-map parsed-org 'link
        (lambda (link)
          (when (string= (org-element-property :type link) "id")
            (let* ((post-blank (org-element-property :post-blank link))
                   (content (org-element-interpret-data (org-element-contents link)))
                   (replacement (concat content (string-pad "" post-blank))))
              (debug) ;; Add a breakpoint here
              (org-element-insert-before link replacement)
              (org-element-extract-element link)))))))

  ;; (add-hook 'org-export-before-processing-functions 'tetov/remove-id-links)

  (defun tetov/clock-in-to-prog (KW)
    "Switch a task from TODO to PROG when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from PROG back to TODO.
Based on bh/clock-in-to-next."
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO"))
             (bh/is-task-p))
        "PROG")
       ((and (member (org-get-todo-state) (list "PROG"))
             (bh/is-project-p))
        "TODO"))))

;;;; general
  (setq org-startup-folded t
        org-startup-indented t
        org-insert-heading-respect-content t)

;;;; tags
  (add-to-list 'org-tags-exclude-from-inheritance "project")
  (add-to-list 'org-tags-exclude-from-inheritance "ATTACH")
  (add-to-list 'org-tags-exclude-from-inheritance "REFILE")
  (add-to-list 'org-tags-exclude-from-inheritance "EMAIL")

;;;; crypt
  (setq org-crypt-key "anton@tetov.se"
        org-crypt-disable-auto-save t)

  (org-crypt-use-before-save-magic)
  (add-to-list 'org-tags-exclude-from-inheritance "crypt")

;;;; refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-target-verify-function #'bh/verify-refile-target

;;;; org id
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (add-hook 'org-capture-before-finalize-hook #'org-id-get-create)

;;;; todo setup
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "WAIT(w@/!)" "|" "CANC(c@/!)" "MEETING" "PHONE"))
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-use-fast-todo-selection t
        org-log-state-notes-into-drawer t

;;;; projects setup
        bh/organization-task-id "a5b03c9e-2390-4ebe-9282-fa901a564a17"

;;;; Tags with fast selection keys
        org-tag-alist (quote ((:startgroup)
                              ("@work" . ?o)
                              ("@home" . ?H)
                              (:endgroup)
                              ("rp" . ?r)))

;;;; clocks (before after!)
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Change tasks to NEXT when clocking in
        org-clock-in-switch-to-state 'tetov/clock-in-to-prog
        ;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;; Do not prompt to resume an active clock
        org-clock-persist-query-resume nil
        ;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        ;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;; don't ask if clock out when closing emacs (since it's probably just a restart)
        org-clock-ask-before-exiting nil
        org-clocktable-defaults '(:maxlevel 2
                                  :lang "en"
                                  :scope agenda
                                  :block lastweek
                                  :fileskip0 t
                                  :match nil
                                  :emphasize t
                                  :link nil
                                  :hidefiles t
                                  :match "-rp"
                                  :step day))


  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  ;; (map! :map org-mode-map
  ;;       :localleader
  ;;       (:prefix ("c" . "clock")
  ;;        :desc "Insert past clock" "p" #'org-insert-past-clock))

  (add-hook 'org-clock-out-hook #'bh/remove-empty-drawer-on-clock-out)
  (add-hook 'org-clock-out-hook #'bh/clock-out-maybe)

;;;; capture
  (setq org-capture-templates `(("d" "default" entry (file org-default-notes-file)
                                 "* TODO %?\n")
                                ("m" "Meeting" entry (file org-default-notes-file)
                                 "* TODO %u Möte %? :MEETING:\n"
                                 :clock-in t
                                 :clock-keep t)
                                ("e" "Email" entry (file org-default-notes-file)
                                 "* TODO %? :EMAIL:
:PROPERTIES:
:from: %:from
:to: %:to
:subject: %:subject
:maildir-at-capture: %:maildir
:message-id: %:message-id
:received: %:date
:END:
%:fromname: %a")))

  (defun tetov/add-property-with-date-captured ()
    "Add DATE_CAPTURED property to the current item.
From https://emacs.stackexchange.com/a/26120/40644"
    (interactive)
    (org-set-property "CAPTURED" (format-time-string "[%F %a %R]")))

  (add-hook 'org-capture-before-finalize-hook #'tetov/add-property-with-date-captured)

;;;;; org-agenda
  (add-hook 'org-agenda-finalize-hook #'org-agenda-show-clocking-issues)
  (setq org-agenda-include-diary t

        org-agenda-sticky t ;; keep agenda view alive

        org-agenda-include-diary t
        org-agenda-dim-blocked-tasks nil
        org-agenda-span 7

        org-agenda-start-with-log-mode t
;;;;; agenda clock consistency
        org-agenda-clock-consistency-checks '(:max-duration "7:00"
                                              :min-duration 0
                                              :max-gap 5
                                              :gap-ok-around ("4:00" "12:30")))

;;;;; agenda views
  (setq org-agenda-custom-commands
        `(("d" "default"
           ((agenda "" nil)))
          ("x" "extended"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to refile")
                   (org-tags-match-list-sublevels nil)))
            (org-ql-block '(and (project)
                                (not (or (descendants (todo "PROG" "WAIT" "NEXT"))
                                         (descendants (planning)))))
                          ((org-ql-block-header "Stuck projects")))
            (org-ql-block '(and (todo "PROG")
                                (not (descendants (todo "PROG" "NEXT"))))
                          ((org-ql-block-header "Current tasks")))
            (org-ql-block '(and (todo "NEXT")
                                (not (descendants (todo "PROG" "NEXT"))))
                          ((org-ql-block-header "Next tasks")))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")))
            (org-ql-block '(and (todo "TODO")
                                (descendants (todo)))
                          ((org-ql-block-header "All projects")))
            (org-ql-block '(and (todo)
                                (not (todo "PROG" "NEXT" "WAIT"))
                                (not (and (todo "TODO")
                                          (descendants (todo)))))
                          ((org-ql-block-header "Other TODOs")))))))


;;;;; setup org-ql
  (use-package! org-ql)
  (org-ql-defpred project ()
    "Find tasks that are projects.

     Tasks with subtasks and tasks categorised as project"
    :body (and (todo "TODO")
               (descendants (todo))))

  (org-ql-defpred stuck ()
    "Find projects that are stuck.

    No NEXT or PROG task in subtree, nor a TODO with a scheduled/deadline
    timestamp."
    :body (and (project)
               (not (or (descendants (todo "PROG" "WAIT" "NEXT"))
                        (descendants (planning))))))


;;;;; org-roam
  (when (tetov-is-android-p)
    ;; fix display issue on phone (wrong width in mini-buffer)
    (setq org-roam-node-display-template "${title}"))

;;;;;; roam capture templates
  (setq org-roam-capture-templates
        `(("n" "note" plain "%?"
           :target (file+head "notes/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: note
:END:
#+title: ${title}
")
           :immediate-finish t
           :unnarrowed t)
          ("p" "person" plain  "%?"
           :target (file+head "persons/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: person
:END:
#+title: ${title}
")
           :immediate-finish t
           :unnarrowed t)
          ("w" "writing" plain  "%?"
           :target (file+head "writing/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: writing
:END:
#+title: ${title}
")

           :immediate-finish nil
           :unnarrowed t)
          ("b" "bibliography reference" plain "%?"
           :target (file+head "refs/${citar-citekey}.org"
                              ":PROPERTIES:
:CATEGORY: reference
:type: ${citar-type}
:authors: ${citar-author}
:date: ${citar-date}
:END:
#+title: ${citar-author} :: ${citar-title}")
           :unnarrowed t
           :immediate-finish t)
          ("r" "org roam ref" plain "%?"
           :target (file+head "refs/${slug}.org" ":PROPERTIES:
:CATEGORY: reference
:END:
#+title: ${title}
")
           :immediate-finish
           :unnarrowed t)
          ("o" "rp notes (Eat Flay Prowl)" plain "%?"
           :target (file+head "rp/${slug}.org"
                              ":PROPERTIES:
:CATEGORY: rp
:END:
#+FILETAGS: :dnd5e:eat-flay-prowl:privat:
#+title: ${title}
")
           :immediate-finish t
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        `(("l" "org roam protocol ref" plain "%?"
           :target (file+head "refs/${slug}.org" ":PROPERTIES:
:CATEGORY: reference
:END:
#+title: ${title}
${body}
")

           :immediate-finish t)

          (("d" "default" entry "* %?"
            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))

          :immediate-finish t
          ))

;;;; references
  (setq citar-org-roam-capture-template-key "b")

;;;;;; org export
  (setq org-export-initial-scope 'buffer
        org-export-with-author nil
        org-export-with-broken-links 'mark
        org-export-with-date nil
        org-export-with-email nil
        org-export-with-tags nil
        org-export-with-timestamps nil
        org-export-with-title t
        org-export-with-toc nil
        org-export-with-todo-keywords nil

;;;;;; latex
        org-latex-default-class "article"
        org-latex-compiler "xelatex"
        org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")

;;;;;; ox-pandoc
        org-pandoc-options '((standalone . t))
        org-pandoc-options-for-latex-pdf `((standalone . t)
                                           (pdf-engine . "xelatex")
                                           ()))
;;;;;; ox-hugo
        org-hugo-export-with-toc nil
        org-hugo-date-format "%Y-%m-%d"
        org-hugo-front-matter-format "yaml"
        org-hugo-goldmark t
        org-hugo-section "posts"
        org-hugo-base-dir "~/src/web/xyz/content/posts"))

(defun tetov/clock-sum-for-tag ()
  "Print the total clocked time for a specific tag between START-DATE and END-DATE."
  (interactive)
  (let* ((start-date (org-read-date nil nil nil "Start date: "))
         (end-date (org-read-date nil nil nil "End date: "))
         (tag (completing-read "Tag: " (org-global-tags-completion-table)))
         (org-agenda-tag-filter-preset (list (concat "+" tag)))
         (org-agenda-files (org-agenda-files))
         (org-agenda-buffer-tmp-name (generate-new-buffer-name "*Org Clock Sum Temp*"))
         clocktable)

    ;; Clear any other active agenda filters
    (org-agenda-filter-apply nil 'tag)

    ;; Generate clocktable in a temporary buffer
    (with-temp-buffer
      (rename-buffer org-agenda-buffer-tmp-name t)
      (org-create-dblock
       (list :type "clocktable"
             :name "clocktable"
             :scope (org-agenda-files) ; Use the list of Org agenda files
             :maxlevel 6
             :tstart start-date
             :tend end-date
             :tags tag))
      (org-dblock-update)
      (write-file "/tmp/debug-clocktable.org")
      (setq clocktable (buffer-string)))
    ;; Extract total time from clocktable
    (if (and (string-match "^.+\\(|\\s-+\\([0-9]+:[0-9]+\\)\\s-+|\\)$" clocktable)
             (match-string 2 clocktable))
        (let ((total-time (match-string 2 clocktable)))
          (message "Total clocked time for tag '%s' between %s and %s: %s"
                   tag start-date end-date total-time))
      (message "Failed to extract total time. Maybe there are no clocked entries for this tag and time range?"))))

(defun tetov/org-agenda-clock-tag ()
  "Display an Org agenda clock report for entries with a specified tag.
The report covers entries clocked between START-DATE and END-DATE."
  (interactive)

  ;; Set the agenda time span to the desired range
  (let* ((start-date (org-read-date nil nil nil "Start date: "))
         (end-date (org-read-date nil nil nil "End date: "))
         (tag (completing-read "Tag: " (org-global-tags-completion-table)))
         (org-agenda-start-day start-date)
         (org-agenda-span (1+ (- (time-to-days (org-time-string-to-time end-date))
                                 (time-to-days (org-time-string-to-time start-date)))))
         (org-agenda-show-log 'clockcheck)
         (org-agenda-clockreport-mode t)
         (org-agenda-log-mode-items '(clock))
         (org-agenda-tag-filter-preset (list (concat "+" tag))))

    ;; Open the agenda view
    (if (get-buffer org-agenda-buffer-name)
        (progn
          (switch-to-buffer org-agenda-buffer-name)
          (org-agenda-redo))
      (org-agenda nil "a"))))

(defun consult-clock-in (&optional match scope resolve)
  "Clock into an Org heading."
  (interactive (list nil nil current-prefix-arg))
  (require 'org-clock)
  (org-clock-load)
  (save-window-excursion
    (consult-org-heading
     match
     (or scope
         (thread-last org-clock-history
                      (mapcar 'marker-buffer)
                      (mapcar 'buffer-file-name)
                      (delete-dups)
                      (delq nil))
         (user-error "No recent clocked tasks")))
    (org-clock-in nil (when resolve
                        (org-resolve-clocks)
                        (org-read-date t t)))))

(after! consult
  (consult-customize consult-clock-in
                     :prompt "Clock in: "
                     :preview-key "M-."
                     :group
                     (lambda (cand transform)
                       (let* ((marker (get-text-property 0 'consult--candidate cand))
                              (name (if (member marker org-clock-history)
                                        "*Recent*"
                                      (buffer-name (marker-buffer marker)))))
                         (if transform (substring cand (1+ (length name))) name)))))

(setq tetov/site-src (expand-file-name "~/src/tetov.se"))

(defun tetov/org-pandoc-export-to-site (&optional a s v b e)
  "Export current buffer to site in GFM format"
  (interactive)
  (unless (boundp 'tetov/site-src)
    (error "You must define the variable 'tetov/site-src' in your configuration."))
  (let ((relative-path (org-entry-get nil "EXPORT_SITE_SRC_PATH" t)))
    (unless relative-path
      (error "You must specify EXPORT_SITE_SRC_PATH in the Org file."))
    ;; Preserve user's current buffer and location.
    (save-excursion
      ;; Retrieve the export path from the Org file.
      ;; Calculate the absolute desired path.
      (let ((desired-path (file-name-concat tetov/site-src relative-path)))

        ;; Export the Org content to a new GFM buffer.
        (org-pandoc-export 'gfm a s v b e t)

        ;; Save the buffer content to the desired file location.
        (write-file desired-path)
        ;; Display a message about the file write.
        (message "Exported to %s" desired-path)))))

;;;;; org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  :hook (after-init . org-roam-ui-mode)
  :config (setq org-roam-ui-sync-theme t
                org-roam-ui-follow nil
                org-roam-ui-update-on-save t
                org-roam-ui-open-on-start nil))
;; needed because roam messes up , binding?
;; https://github.com/doomemacs/doomemacs/issues/4242#issuecomment-731436096
(add-hook! 'org-mode-hook #'+org-init-keybinds-h)

;;;; backup
(use-package! backup-each-save
  :config
  (require 'tetov-editor-backup)
  (setq backup-each-save-mirror-location (tetov-editor-backup-setup-dir tetov/nextcloud-apps-dir)
        backup-each-save-remote-files t
        backup-each-save-filter-function 'tetov-editor-backup-each-save-filter
        ;; save to same file for 1 hour
        backup-each-save-time-format "%Y_%m_%d_%H_00_00"))
(add-hook 'after-save-hook #'backup-each-save)

(auto-save-visited-mode 1)
;;;; coding
;;;;; lsp
(after! lsp
  ;; fix problem with flycheck-next-error-function != flycheck-next-error
  (advice-add 'next-error :override 'flycheck-next-error)
  (advice-add 'previous-error :override 'flycheck-previous-error))

(add-hook 'ansible-hook #'lsp!)

;;;;; projectile
(after! projectile
  (setq projectile-auto-discover t
        projectile-project-search-path '(("~/src" . 2))))  ;; number means search depth

;;;;; python
;; use format-all, not lsp formatter
(setq-hook! 'python-mode-hook +format-with-lsp nil)
(after! python
  (setq poetry-tracking-strategy 'projectile))

;;;; writing

;;;;; spelling
;; based on https://200ok.ch/posts/2020-08-22_setting_up_spell_checking_with_multiple_dictionaries.html
(after! ispell
  (let* ((default-dict "en_US")
         (extra-dict "sv_SE")
         (use-multi-dict (member extra-dict (ispell-valid-dictionary-list))))
    (progn
      ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
      ;; dictionary' even though multiple dictionaries will be configured
      ;; in next line.
      (setenv "LANG" (format "%s.UTF-8" default-dict))
      (setq ispell-dictionary (if use-multi-dict
                                  (format "%s,%s" extra-dict default-dict)
                                default-dict)
            ispell-local-dictionary-alist
            `((,ispell-dictionary "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" ,ispell-dictionary) nil utf-8))
            ;; For saving words to the personal dictionary, don't infer it from
            ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
            ispell-personal-dictionary "~/.hunspell_personal")
      ;; The personal dictionary file has to exist, otherwise hunspell will
      ;; silently not use it.
      (unless (file-exists-p ispell-personal-dictionary)
        (write-region "" nil ispell-personal-dictionary nil 0))
      ;; ispell-set-spellchecker-params has to be called
      ;; before ispell-hunspell-add-multi-dic will work
      (ispell-set-spellchecker-params)
      (when use-multi-dict
        (ispell-hunspell-add-multi-dic ispell-dictionary)))))

;;;; mail - contacts - calendar

;;;;; mail
(defun tetov/mu4e-refile-folder-function (msg)
  "Set the refile directory for message.

   Refile with this function means moving msg from INBOX to Archive. If the msg
   is not in INBOX leave it be (set refile dir to current dir it is in)."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (string-replace "/INBOX" "/Archive" maildir)))

(set-email-account! "fastmail"
                    '((user-mail-address      . "anton@tetov.se")
                      (mu4e-sent-folder       . "/fastmail/Sent")
                      (mu4e-drafts-folder     . "/fastmail/Drafts")
                      (mu4e-trash-folder      . "/fastmail/Trash")
                      (smtpmail-smtp-user     . "tetov@fastmail.com")
                      (+mu4e-personal-addresses . '("anton@tetov.se"))
                      (mu4e-compose-signature . "Best regards\nAnton Tetov Johansson"))
                    t)

(set-email-account! "lth"
                    '((user-mail-address       . "anton_tetov.johansson@abm.lth.se")
                      (mu4e-sent-folder        . "/lth/Sent Items")
                      (mu4e-drafts-folder      . "/lth/Drafts")
                      (mu4e-trash-folder       . "/lth/Deleted Items")
                      (smtpmail-smtp-user      . "anton_tetov.johansson@abm.lth.se")
                      (+mu4e-personal-addresses . '("anton_tetov.johansson@abm.lth.se"))
                      (mu4e-compose-signature  . "Best regards,
Anton Tetov Johansson (they/them)
PhD student in Architecture & Construction Robotics

Department of Architecture & Built Environment
Lund University - LTH

Phone no: +46 46-222 71 11

https://abm.lth.se/
https://tetov.se/"))
                    nil)

(after! mu4e
;;;;;; send/recieve email
  ;; mail box updated using systemd timer, so mail command is set to true
  ;; mu4e still indexes again but that should be fine.
  (setq mu4e-get-mail-command "true"
        sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        +mu4e-compose-org-msg-toggle-next nil

;;;;;; headers view options
        mu4e-headers-fields
        '((:account-stripe . 1)
          (:human-date . 12)
          (:flags . 6)
          (:from-or-to . 25)
          (:maildir . 20)
          (:subject))

;;;;;; message view options

        ;; make html mails more readable in dark mode
        shr-color-visible-luminance-min 80
        ;; make buttons to change between mail format types
        gnus-unbuttonized-mime-types nil
        mu4e-headers-skip-duplicates nil
;;;;;; compose view options
        mu4e-compose-in-new-frame t
        mu4e-compose-context-policy 'ask
        message-citation-line-format "On %Y-%m-%d at %R, %f wrote:"
        message-citation-line-function  #'message-insert-formatted-citation-line
        mu4e-refile-folder 'tetov/mu4e-refile-folder-function

        mu4e-change-filenames-when-moving t
        mu4e-attachment-dir (if (tetov-is-wsl-p)
                                (file-name-concat tetov/win-user-dir "Downloads")
                              (expand-file-name "~/Downloads"))
        +org-capture-emails-file org-default-notes-file)

;;;;;; folding
  (setq mu4e-folding-default-view 'folded)
  (use-package! mu4e-folding)
  (add-hook 'mu4e-headers-mode-hook 'mu4e-folding-mode)
  (set-face-attribute 'mu4e-folding-root-folded-face nil :weight 'ultra-bold :background "gray20")
  (set-face-attribute 'mu4e-folding-root-unfolded-face nil :weight 'ultra-bold :background "gray20")
  (set-face-attribute 'mu4e-folding-child-unfolded-face nil :background "gray20")
  (set-face-attribute 'mu4e-folding-child-folded-face nil :background "gray20")

  (define-key mu4e-headers-mode-map "zo" 'mu4e-folding-toggle-at-point)
  (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-folding-toggle-at-point)

  (define-key mu4e-headers-mode-map "zc" 'mu4e-folding-fold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-folding-fold-at-point)

  (define-key mu4e-headers-mode-map "zm" 'mu4e-folding-fold-all)
  (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-folding-fold-all)

  (define-key mu4e-headers-mode-map "zo" 'mu4e-folding-unfold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-folding-unfold-at-point)

  (define-key mu4e-headers-mode-map "zr" 'mu4e-folding-unfold-all)
  (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-folding-unfold-all)

  (set-company-backend! 'mu4e-compose-mode 'company-capf)
  (add-to-list 'mu4e-bookmarks
               ;; add bookmark for recent messages on the Mu mailing list.
               '( :name "allinboxes"
                  :key  ?i
                  :query "maildir:/lth/INBOX OR maildir:/fastmail/INBOX"))

;;;;;; mark threads
  (map! :localleader :map 'mu4e-view-mode-map :desc "Mark thread" "t" #'mu4e-view-mark-thread)
  (map! :localleader :map 'mu4e-headers-mode-map :desc "Mark thread" "t" #'mu4e-headers-mark-thread))

;;;;;; start automatically
(unless (tetov-is-android-p)
  (run-at-time "5 sec" nil (lambda ()
                             (let ((current-prefix-arg '(4)))
                               (call-interactively 'mu4e)
                               (message nil)))))

;;;;; contacts
(setq vdirel-repository (substitute-in-file-name "$XDG_DATA_HOME/vdirsyncer/contacts/Default"))

;;;;; calendars
(add-hook 'diary-mark-entries-hook #'diary-mark-included-diary-files)

(setq excorporate-configuration '("an6802jo@lu.se" . "https://webmail.lu.se/EWS/Exchange.asmx")
      excorporate-calendar-show-day-function #'exco-calfw-show-day)
;;(use-package! excorporate)

;;;; term
;; https://github.com/syl20bnr/spacemacs/issues/2345#issuecomment-240634646
(after! vterm
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t))

(after! eshell
  (set-eshell-alias!
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
   "gr" "g remote $*"
   "gst" "g status $*"

   "cm" "chezmoi $*"

   "cmcd" "eshell/cd ${chezmoi source-path}"))

;;;; chezmoi
(use-package! chezmoi)
;; :config
;; (add-hook 'chezmoi-mode-hook
;;           #'(lambda () (if chezmoi-mode
;;                            (add-to-list 'company-backends 'chezmoi-company-backend)
;;                          (setq company-backends (delete 'chezmoi-company-backend company-backends))))))

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

;;;; pdf-tools
(after! pdf-tools
  (setq pdf-view-continuous nil))

;;;; elfeed (RSS)
(after! elfeed
  (setq elfeed-use-curl t
        elfeed-feeds '("owncloud+https://tetov@cloud.tetov.se")
        elfeed-protocol-enabled-protocols '(owncloud))
  (elfeed-set-timeout 36000)
  (elfeed-protocol-enable))

;;;; pocket-reader
(setq org-pocket-capture-file org-default-notes-file)
(after! pocket-reader
  (require 'org-pocket))

;; taskjuggler
;;(require 'taskjuggler-mode)

;; rust
;;(use-package rustic
;;  :custom
;;  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

;; 1pasword
(use-package! 1password
  :demand t
  :init
  (message "Enabling 1password ...")
  :config
  (1password-auth-source-enable))
