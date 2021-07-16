;; * Look and feel

;; ** Start maximised (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; ** Don't ask to quit
(setq confirm-kill-emacs nil)

;; ** Modeline adjustments
(setq doom-modeline-major-mode-icon t)

;; ** lsp always show breadcrumb
(setq lsp-headerline-breadcrumb-enable t)

;; ** doom-gruvbox for the theme
(setq
 doom-theme 'doom-gruvbox
 display-line-numbers-type 'relative)

;; ** error in treemacs icons
(doom-themes-treemacs-config)
(after! treemacs
  (treemacs-load-theme "doom-colors"))

;; ** Outshine mini mode for all major modes
(add-hook 'prog-mode-hook 'outshine-mode)

;; ** Font
(setq doom-font (font-spec :family "Source Code Pro Medium" :size 15))


;; * Languages

;; ** Python
;; *** Enable dap-mode for python
(add-hook 'python-mode (require 'dap-python))


;; * Keybinds

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
      :i "C-j" #'next-line
      :i "C-p" #'previous-line
      :i "C-n" #'next-line)

;; ** jump to paranthesis with tab
(map! :n [tab] 'evil-jump-item)

;; ** expand region
(map! :leader
      (:desc "Expand region"  "v" #'er/expand-region))


;; * Misc
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

;; ** dired hide files toggle on M-h
(setq my-dired-ls-switches "-alh --ignore=.* --ignore=\\#* --ignore=*~")

(setq my-dired-switch 1)

(add-hook 'dired-mode-hook
        (lambda ()
        "Set the right mode for new dired buffers."
        (when (= my-dired-switch 1)
        (dired-sort-other my-dired-ls-switches))))

(add-hook 'dired-mode-hook
        (lambda ()
        (define-key dired-mode-map (kbd "M-h")
        (lambda ()
                "Toggle between hide and show."
                (interactive)
                (setq my-dired-switch (- my-dired-switch))
                (if (= my-dired-switch 1)
                (dired-sort-other my-dired-ls-switches)
                (dired-sort-other "-alh"))))))
