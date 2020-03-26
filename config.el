;; whoami

(setq user-full-name "Soham Chowdhury"
      user-mail-address "evertedsphere@gmail.com")

;; basic appearance settings

(setq doom-theme 'doom-palenight)
(setq doom-vibrant-padded-modeline nil)
(setq doom-font (font-spec :family "PragmataPro Liga" :size 20)
      doom-big-font (font-spec :family "PragmataPro Liga" :size 30)
      doom-variable-pitch-font (font-spec :family "PragmataPro Liga"))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(setq treemacs-width 30)
;; for perf
(setq inhibit-compacting-font-caches t)
(setq display-line-numbers-type nil)
(global-company-mode +1)

;; fundamental keybinds

(setq doom-localleader-key ",")

;; custom modeline

(after! doom-modeline
  (doom-modeline-def-modeline 'evertedsphere/modeline
    '(bar battery misc-info matches buffer-info remote-host buffer-position)
    ;; HACK Move flycheck away from the extreme right end to prevent clipping.
    '(lsp checker minor-modes major-mode process vcs))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'evertedsphere/modeline 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))

;; Header line (disabled)
;; (defun evertedsphere/capture-shell-output (cmd)
;;   (substring
;;    (shell-command-to-string cmd)
;;    0 -1))
;; (load-file "~/.doom.d/header.el")

;; jethrokuan's isearch optimisations

(setq search-highlight t search-whitespace-regexp ".*?"
      isearch-lax-whitespace t isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

(global-subword-mode 1)

;; org, deft, org-roam, etc.

(setq deft-directory "~/org/"
      deft-recursive t)

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-hugo-default-section-directory "posts")
  ;; (add-hook 'org-mode-hook #'writeroom-mode)
  ;; (add-to-list 'org-modules 'org-protocol t)

  (setq org-bullets-bullet-list '("·" ":" "𐬼"))
  (setq org-directory "~/org/")

  (require 'find-lisp)
  (setq evertedsphere/org-agenda-directory "~/org/agenda/")
  (setq org-agenda-files
        (find-lisp-find-files evertedsphere/org-agenda-directory "\.org$"))

  (setq org-capture-templates
        `(("i" "inbox" entry
           (file ,(concat evertedsphere/org-agenda-directory "inbox.org"))
           "* TODO %?")
          ("w" "review" entry
           (file+olp+datetree ,(concat evertedsphere/org-agenda-directory
                                       "reviews.org"))
           (file ,(concat evertedsphere/org-agenda-directory
                          "templates/weekly-review.org")))
          ("e" "email" entry
           (file+headline
            ,(concat evertedsphere/org-agenda-directory "emails.org") "Emails")
           "* TODO [#A] Reply: %a :@home:@school:"
           :immediate-finish t)
          ("c" "org-protocol-capture" entry
           (file ,(concat evertedsphere/org-agenda-directory "inbox.org"))
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil)

  (setq org-tag-alist (quote (("@office" . ?o)
                              ("@home" . ?h)
                              (:newline)
                              ("WAITING" . ?w)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?c))))

  (setq org-fast-tag-selection-single-key nil)
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 0)
                             ("reading.org" :level . 1)
                             ("projects.org" :maxlevel . 1)))
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)

  (defun evertedsphere/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  (map! "<f1>" #'evertedsphere/switch-to-agenda)

  (defun jethro/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (map! :map org-agenda-mode-map
        "i" #'org-agenda-clock-in
        "R" #'org-agenda-refile
        "c" #'evertedsphere/org-inbox-capture)

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands
        `((" " "Agenda"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-deadline-warning-days 365)))
            (todo "TODO"
                  ((org-agenda-overriding-header "To refile")
                   (org-agenda-files '(,(concat evertedsphere/org-agenda-directory "inbox.org")))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Emails")
                   (org-agenda-files '(,(concat evertedsphere/org-agenda-directory "emails.org")))))
            (todo "NEXT"
                  ((org-agenda-overriding-header "In progress")
                   (org-agenda-files '(,(concat evertedsphere/org-agenda-directory "someday.org")
                                       ,(concat evertedsphere/org-agenda-directory "projects.org")
                                       ,(concat evertedsphere/org-agenda-directory "next.org")))
                   ))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '(,(concat evertedsphere/org-agenda-directory "projects.org")))
                   ))
            (todo "TODO"
                  ((org-agenda-overriding-header "One-off tasks")
                   (org-agenda-files '(,(concat evertedsphere/org-agenda-directory "next.org")))
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert)
  (setq org-roam-directory "~/org/zettels"
        org-roam-db-location "~/org/.org-roam.db")
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo-setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))

(use-package company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(after! (org org-roam)
    (defun my/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- [[file:%s][%s]]\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-sql [:select [file-from]
                             :from file-links
                             :where (= file-to $s1)
                             :and file-from :not :like $s2] file "%private%"))
        ""))
    (defun my/org-export-preprocessor (_backend)
      (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))

(after! (org ox-hugo)
  (defun jethro/conditional-hugo-enable ()
    (save-excursion
      (if (cdr (assoc "SETUPFILE" (org-roam--extract-global-props '("SETUPFILE"))))
          (org-hugo-auto-export-mode +1)
        (org-hugo-auto-export-mode -1))))
  (add-hook 'org-mode-hook #'jethro/conditional-hugo-enable))

(use-package! org-ref-ox-hugo
  :after org org-ref ox-hugo
  :config
  (add-to-list 'org-ref-formatted-citation-formats
               '("md"
                 ("article" . "${author}, *${title}*, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
                 ("inproceedings" . "${author}, *${title}*, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("book" . "${author}, *${title}* (${year}), ${address}: ${publisher}.")
                 ("phdthesis" . "${author}, *${title}* (Doctoral dissertation) (${year}). ${school}, ${address}.")
                 ("inbook" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("incollection" . "${author}, *${title}*, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
                 ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
                 ("unpublished" . "${author}, *${title}* (${year}). Unpublished manuscript.")
                 ("misc" . "${author} (${year}). *${title}*. Retrieved from [${howpublished}](${howpublished}). ${note}.")
                 (nil . "${author}, *${title}* (${year})."))))

;; TODO fix
;; (use-package! srefactor
;;   :commands (srefactor-lisp-format-buffer))

(map! :leader
       (:prefix-map ("b" . "buffer")
        :desc "Previous buffer"             "h"   #'previous-buffer
        :desc "Next buffer"                 "l"   #'next-buffer))

(after! ivy
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-delete-char))

(setq haskell-process-type 'cabal-new-repl)

(after! lsp-haskell
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '()))

;; TODO shouldn't this be SPC m l a etc instead of SPC l a
(map! :leader
      (:after lsp-mode
        (:prefix ("l" . "LSP")
          :desc "Excute code action" "a" #'lsp-execute-code-action
          :desc "Go to definition" "d" #'lsp-find-definition
          :desc "Glance at doc" "g" #'lsp-ui-doc-glance
          (:prefix ("u" . "LSP UI")
            :desc "Toggle doc mode" "d" #'lsp-ui-doc-mode
            :desc "Toggle sideline mode"  "s" #'lsp-ui-sideline-mode
            :desc "Glance at doc" "g" #'lsp-ui-doc-glance
            :desc "Toggle imenu"  "i" #'lsp-ui-imenu))))

(after! haskell-mode
  (set-formatter! 'ormolu "ormolu"
    :modes '(haskell-mode)))

(after! rustic
  (setq rustic-lsp-server 'rust-analyzer))

(setq web-mode-enable-engine-detection t)
