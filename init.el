;;; init.el -*- lexical-binding: t; -*-

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;chinese
       ;;japanese

       :desktop
       exwm

       :completion
       ;; Using a child frame gives us those cute icons beside the candidates.
       (company +childframe)
       (ivy +prescient)

       :ui
       deft
       doom
       modeline
       maxibuffer
       fill-column
       hl-todo
       indent-guides
       hydra
       nav-flash
       ophints
       (popup +all +defaults)
       pretty-code
       treemacs
       unicode
       ;; vc-gutter
       (window-select +switch-window)
       workspaces
       zen

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates
       fold
       (format +onsave)
       ;;god
       lispy
       multiple-cursors
       parinfer
       rotate-text
       snippets
       word-wrap

       :emacs
       (dired +ranger)
       electric
       ibuffer
       vc

       :term
       eshell
       ;;shell
       ;; term
       vterm

       :checkers
       syntax
       ;;spell
       ;;grammar

       :tools
       ;;ansible
       ;;debugger
       direnv
       ;;docker
       editorconfig
       ;;ein
       (eval +overlay)
       gist
       lookup
       (lsp +peek)
       magit
       make
       pass
       pdf
       ;;prodigy
       rgb
       ;;terraform
       tmux
       ;;upload

       :lang
       (agda +local)
       assembly
       (cc +lsp)
       common-lisp
       data
       emacs-lisp
       (haskell +lsp)
       idris
       (java +meghanada)
       (javascript +lsp)
       (kotlin +lsp)
       (latex +latexmk +cdlatex)
       lean
       ledger
       ;;lua
       markdown
       nix
       (org
        +export
        +hugo
        +babel
        +publish
        +capture
        ;; +dragndrop
        ;; +pandoc
        +pomodoro
        +present)
       plantuml
       purescript
       python
       racket
       rest
       rst
       ;;(ruby +rails)
       (rust +lsp)
       ;;scala
       scheme
       sh
       ;;solidity
       ;;swift
       ;;terra
       (web +html +css)

       :email
       ;; (mu4e +gmail)
       notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       irc
       ;;(rss +org)
       twitter

       :config
       (default +bindings +smartparens +snippets))
