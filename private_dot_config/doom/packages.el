;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
(package! org :pin "ca873f7fe47546bca19821f1578a6ab95bf5351c5890aca")
;; (unpin! org-roam) ;; for orb and org-roam-ui
(package! org-roam-ui)

(package! backup-each-save)
;; (package! burly)
(package! chezmoi)
(package! elfeed-protocol)
(package! excorporate)
(package! org-mime)
(package! org-clock-convenience)
(package! org-ql)
(package! org-ref)
(package! org-wc)
;; (package! pocket-reader)
;; (package! scad-mode)
;; (package! vdirel)

;; (package! org-pocket
;;   :recipe (:host github :repo "alphapapa/org-pocket"))

(package! mu4e-folding
  :recipe (:host github :repo "rougier/mu4e-folding"))

;; org clock
(package! org-clock-helpers
  :recipe (:repo "mskorzhinskiy/org-clock-helpers"
           :host github :branch "main"))

(package! tetov
  :recipe (:host sourcehut :repo "tetov/tetov.el" :files ("src/*.el")))
