;; * Look and feel

;; ** Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(setq
  doom-theme 'doom-one
  display-line-numbers-type 'relative
)
(add-hook 'after-init-hook 'global-company-mode)

;; ** Outshine mini mode for all major modes
(add-hook 'prog-mode-hook 'outshine-mode)

;; * Languages

;; ** Python

;; *** Custom yasnippet: https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style

;; **** Docstring
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

;; ** Javascript

;; *** Javascript prettier
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

;; * Keybinds

;; ** Autocomplete cycle through completions
(map! :map ac-completing-map "C-j" #'ac-next)
(map! :map ac-completing-map "C-k" #'ac-previous)

;; ** Company pick from list
;; (define-key company-active-map (kbd "C-SPC") #'company-complete-selection)
(map! :map company-active-map "C-SPC" #'company-complete)
;; (map! :map company-active-map "RET" #'company-complete)
;; (map! :map company-active-map [return] #'company-complete)

;; ** Docker-compose

(map! :leader
 (:desc "Docker"  "d" #'docker)
 )

;; ** rgrep in project
(map! :leader
 (:prefix-map ("s" . "search")
        :desc "rgrep in project" "r" #'rgrep)
 )

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

;; Misc

(setq org-directory "~/Documents/ORG/")

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
