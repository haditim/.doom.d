;; * Look and feel

;; ** Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; ** Don't ask to quit
(setq confirm-kill-emacs nil)

;; ** Modeline adjustments
(setq doom-modeline-major-mode-icon t)

;; ** lsp always show breadcrumb
(setq lsp-headerline-breadcrumb-enable t)

;; ** doom-gruvbox for the theme
(setq
 doom-theme 'doom-molokai
 display-line-numbers-type 'relative)

;; ** error in treemacs icons
(doom-themes-treemacs-config)
(after! treemacs
  (treemacs-load-theme "doom-colors")
  (setq treemacs-follow-mode t))

;; ** Outshine mini mode for all major modes
(add-hook 'prog-mode-hook 'outshine-mode)

;; ** Font
(setq doom-font (font-spec :family "Dejavu Sans Mono" :size 15)
      doom-big-font (font-spec :family "Dejavu Sans Mono"  :size 21))

;; * Keybinds
;; ** rgrep in project
(map! :leader
      (:prefix-map ("s" . "search")
       :desc "rgrep in project" "r" #'rgrep))

;; ** jump to paranthesis with tab
(map! :n [tab] 'evil-jump-item)

;; ** expand region
(map! :leader
      (:desc "Expand region"  "v" #'er/expand-region))


;; ** rename buffer
(map! :leader
      (:prefix "b"
       :desc "Rename the buffer" "R" #'rename-buffer))

;; ** debugger
(map! :leader
      (:prefix "o"
       :desc "Debugger start last" "l" #'+debugger/start-last
       :desc "Debugger quit" "q" #'+debugger/quit))

;; ** toggle keycast-mode
(map! :leader
      (:prefix "t"
       :desc "keycast" "k" #'keycast-mode))

;; ** winner undo, redo (undo window configuration)
(map! "C-c <left>" 'winner-undo
      "C-c <right>" 'winner-redo)

;; ** drag text up-down
(map! :n "M-p" 'drag-stuff-up
      :n "M-n" 'drag-stuff-down)

;; ** stop compilation
(map! :leader
      (:prefix "c"
       :desc "Kill compilation" "K" #'kill-compilation))

;; * Misc
(setq org-directory "~/Documents/ORG/")

;; ** enable eldoc-box by default when eglot loads
(add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)

;; ** rgrep ignore some folders
(eval-after-load 'grep
  '(progn
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories ".bundle")
     (add-to-list 'grep-find-ignored-directories "auto")
     (add-to-list 'grep-find-ignored-directories "env")
     (add-to-list 'grep-find-ignored-directories ".env")
     (add-to-list 'grep-find-ignored-directories "venv")
     (add-to-list 'grep-find-ignored-directories ".pytest_cache")
     (add-to-list 'grep-find-ignored-directories "elpa")))
(setq wgrep-enable-key (kbd "C-c C-c"))
(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;; ** dired hide files toggle on M-h
(setq my-dired-ls-switches "-alh --ignore=.* --ignore=\\#* --ignore=*~")

(setq my-dired-switch 1)

;; ** dired don't ask questions about size
(setq large-file-warning-threshold nil)

(add-hook 'dired-mode-hook
          (lambda ()
            "Set the right mode for new dired buffers."
            (when (= my-dired-switch 1)
              (dired-sort-other my-dired-ls-switches))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)  ; enable with "("
            (define-key dired-mode-map (kbd "M-h")
              (lambda ()
                "Toggle between hide and show."
                (interactive)
                (setq my-dired-switch (- my-dired-switch))
                (if (= my-dired-switch 1)
                    (dired-sort-other my-dired-ls-switches)
                  (dired-sort-other "-alh"))))))


;; ** Ansi colors in buffer
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; ** undo-tree everywhere
(setq global-undo-tree-mode t)

;; ** keycast with doom modeline
(after! keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
                (add-hook 'pre-command-hook 'keycast--update t)
                (add-to-list 'global-mode-string '("" mode-line-keycast)))
      (progn
         (remove-hook 'pre-command-hook 'keycast-mode-line-update)
         (setq global-mode-string (delete '("" mode-line-keycast " ") global-mode-string)))))
  (setq keycast-substitute-alist '((evil-next-line nil nil)
                                   (evil-previous-line nil nil)
                                   (evil-forward-char nil nil)
                                   (evil-backward-char nil nil)
                                   (ivy-done nil nil)
                                   (self-insert-command nil nil))))
(add-to-list 'global-mode-string '("" mode-line-keycast))

;; ** Set projects path
(setq projectile-project-search-path '("~/Projects/Code"))

;; ** EAF
;; Still too slow for my taste
;; (add-load-path! "~/.doomemacs.d/site-lisp/emacs-application-framework/")
;; (use-package! eaf
;;   :config
;;         (require 'eaf)
;;         (require 'eaf-browser)
;;         (require 'eaf-pdf-viewer)
;;         (require 'eaf-org-previewer)
;;         (require 'eaf-terminal)
;;         (require 'eaf-video-player)
;;         (require 'eaf-markdown-previewer)
;;         (require 'eaf-image-viewer)
;;         (setq browse-url-browser-function 'eaf-open-browser)
;;         (setq eaf-browser-default-search-engine "startpage")
;;         (setq eaf-browse-blank-page-url "https://startpage.com")
;;         (setq eaf-browser-enable-adblocker "true")
;;         (setq eaf-browser-continue-where-left-off t)
;;         (setq eaf-browser-default-zoom "3")
;;         (when doom-big-font-mode)
;;         (setq eaf-browser-default-zoom 1.5)
;;         (setq eaf-mindmap-dark-mode "follow")
;;         (setq eaf-browser-dark-mode "force")
;;         (setq eaf-terminal-dark-mode "force")
;;         (setq eaf-pdf-dark-mode "force"))
