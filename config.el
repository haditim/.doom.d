;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq
 doom-font (font-spec :family "Source Code Pro" :size 14)
 doom-big-font (font-spec :family "Source Code Pro" :size 22)
 py-autopep8-options '("--max-line-length=120")
 display-line-numbers-type 'relative
 elpy-rpc-virtualenv-path 'current
)

;; Place your private configuration here
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; Jedi
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

(setq-default flycheck-flake8-maximum-line-length 120)
;; enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'after-init-hook 'global-company-mode)

;; issue: https://github.com/hlissner/doom-emacs/issues/2135
(fset 'battery-update #'ignore)

;; keybinds
;; autocomplete cycle through completions
(map! :map ac-completing-map "C-j" #'ac-next)
(map! :map ac-completing-map "C-k" #'ac-previous)
;; Docker-compose
(map! :leader
 (:prefix-map ("d" . "Docker")
  :desc "Docker-compose"  "c" #'docker-compose
  :desc "Docker"  "d" #'docker)
 )
