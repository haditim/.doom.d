;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Source Code Pro" :size 14)
 doom-big-font (font-spec :family "Source Code Pro" :size 22)
 py-autopep8-options '("--max-line-length=120")
 display-line-numbers-type 'relative
 elpy-rpc-virtualenv-path 'current
)
;; Place your private configuration here
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
(setq-default flycheck-flake8-maximum-line-length 120)

;; Enable elpy

(elpy-enable)


;; Enable Flycheck

;; (when (require 'flycheck nil t)

;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))

;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

