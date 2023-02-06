;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

(package! pocket-reader)
(package! backup-each-save)
(package! org-ref)
(package! org-mime)
(package! vdirel)
(package! org-ql)
(package! helm-org-ql)
(package! org-wc)
(package! chezmoi)
(package! elfeed-protocol)

(package! org-pocket
  :recipe (:host github :repo "alphapapa/org-pocket"))

(package! tetov-elisp
  :recipe (:host github :repo "tetov/elisp"
           :files ("*.el")))

(package! mu4e-folding
  :recipe (:host github :repo "rougier/mu4e-folding"))

;; org-roam-bibtex
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam)

;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

;; org clock
(package! org-clock-helpers
  :recipe (:repo "mskorzhinskiy/org-clock-helpers"
           :host github :branch "main"))
