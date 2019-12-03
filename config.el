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
;; rgrep in project
(map! :leader
 (:prefix-map ("s" . "search")
        :desc "rgrep in project" "r" #'rgrep)
 )

;; Custom yasnippets: https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
     (nr 0)
         (formatted-args
      (mapconcat
       (lambda (x)
         (concat "   " (nth 0 x)
             (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
             (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
          (list "" "Args:" formatted-args)
          indent)
       "\n"))))
