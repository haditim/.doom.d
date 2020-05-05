;; * Look and feel
(setq
 doom-font (font-spec :family "Source Code Pro" :size 14)
 doom-big-font (font-spec :family "Source Code Pro" :size 22)
 py-autopep8-options '("--max-line-length=120")
 display-line-numbers-type 'relative
 elpy-rpc-virtualenv-path 'current
)
;; ** Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(add-hook 'after-init-hook 'global-company-mode)

;; ** Outshine mini mode for all major modes
(add-hook 'prog-mode-hook 'outshine-mode)

;; * Languages

;; ** Python

;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;; change max line length for python
(setq-default flycheck-flake8-maximum-line-length 120)

;; *** Enable jedi, elpy and anaconda
(elpy-enable)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional
;; (add-hook 'python-mode-hook 'anaconda-mode)

;; *** Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; ** Javascript

;; *** Javascript prettier
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)


;; * Helpers

;; ** company backends
(defun my/python-mode-hook ()
  ;; (add-to-list 'company-backends 'company-anaconda)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-yasnippet)
  (add-to-list 'company-backends 'elpy-module-company))

;; ** Custom yasnippets: https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
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

;; * Issues

;; ** issue: https://github.com/hlissner/doom-emacs/issues/2135
(fset 'battery-update #'ignore)
;; ** Lisp nesting exceeds error
(setq max-lisp-eval-depth 100000)

;; * Keybinds
;; ** Autocomplete cycle through completions
(map! :map ac-completing-map "C-j" #'ac-next)
(map! :map ac-completing-map "C-k" #'ac-previous)

;; ** Company pick from list
(define-key company-active-map (kbd "C-SPC") #'company-complete-selection)

;; ** Docker-compose
(map! :leader
 (:desc "Docker"  "d" #'docker)
 )
;; ** rgrep in project
(map! :leader
 (:prefix-map ("s" . "search")
        :desc "rgrep in project" "r" #'rgrep)
 )

;; ** Multiple cursor
;; Should focus on evil-mc rather than mc
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; ** Ctrl+vim navigation keys in the evil edit mode
(map! :i "C-l" #'forward-char
      :i "C-h" #'backward-char
      :i "C-k" #'previous-line
      :i "C-j" #'next-line)

;; ** ido bindings
(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k")   'ido-prev-match))

(add-hook 'ido-setup-hook #'bind-ido-keys)

;; ** Mouse multicursor from https://github.com/hlissner/doom-emacs/issues/2174
(defun make-cursor-here-then-move (event)
  (interactive "e")
  (save-excursion
    (let (mouse-drag-and-drop-region)
      (mouse-drag-region event))
    (evil-mc-make-cursor-here)))

(map! "<C-M-mouse-1>" #'make-cursor-here-then-move)

;; * Misc

;; ** rgrep ignore some folders
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "env")
     (add-to-list 'grep-find-ignored-directories "venv")
     (add-to-list 'grep-find-ignored-directories ".pytest_cache")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(setq wgrep-enable-key (kbd "C-c C-c"))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; ** Bash for shell https://emacs.stackexchange.com/questions/28647/how-do-i-change-the-default-shell-for-shell-command
(setq explicit-shell-file-name "/bin/bash")

