;;; init.el -*- lexical-binding: t; -*-
;; to see details and full example visit https://github.com/doomemacs/doomemacs/blob/master/init.example.el
(doom! :input
       bidi

       :completion
       (company +auto +childframe)
       (vertico +icons)

       :ui
       doom
       doom-dashboard
       fill-column
       hl-todo
       hydra
       indent-guides
       modeline
       nav-flash
       ophints
       (popup +defaults)
       ligatures
       (treemacs +icons +lsp)
       vc-gutter
       vi-tilde-fringe
       window-select
       workspaces
       zen
       minimap

       :editor
       (evil +everywhere)
       file-templates
       fold
       format
       multiple-cursors
       parinfer
       rotate-text
       snippets

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       (undo +tree)
       vc

       :term
       term
       vterm

       :checkers
       (syntax +childframe)
       (spell +flyspell +hunspell +eveywhere)
       grammar

       :tools
       ansible
       (debugger +lsp)
       (docker +lsp)
       ein
       (eval +overlay)
       (lookup +dictionary +offline)
       (lsp)
       magit
       make
       pdf
       rgb
       upload
       tree-sitter

       :lang
       (cc +lsp)
       (clojure +lsp)
       crystal
       data
       (elixir +lsp)
       elm
       emacs-lisp
       (graphql +lsp)
       (json +lsp)
       (javascript +lsp +tide)
       (kotlin +lsp)
       markdown
       org
       raku
       php
       (python +lsp +dap)
       rest
       (rust +lsp)
       (scheme +guile)
       (sh +lsp)
       (web +lsp)
       (yaml +lsp)

       :app
       emms

       :config
       literate
       (default +bindings +smartparens))
