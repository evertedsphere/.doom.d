;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! libmpdel)
(package! ivy-mpdel)

(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam"))
(package! org-ql
  :recipe (:host github :repo "alphapapa/org-ql"))
(package! org-clock-convenience)
(package! org-journal)
(package! org-gcal)
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))

(package! spacemacs-theme
  :recipe (:host github :repo "nashamri/spacemacs-theme"))
(package! theme-looper)

;; (package! doom-modeline)
;; (package! mini-modeline)

(unpin! doom-themes)
