(map! :n [tab] 'evil-jump-item)

(map! :leader
      (:desc "Expand region"  "v" #'er/expand-region))

(map! :leader
      (:prefix "b"
       :desc "Rename the buffer" "R" #'rename-buffer))

(map! :leader
      (:prefix "o"
       :desc "Debugger start last" "l" #'+debugger/start-last
       :desc "Debugger quit" "q" #'+debugger/quit))

(map!
 :n "M-p" 'drag-stuff-up
 :n "M-n" 'drag-stuff-down)

(map! :leader
      (:prefix "c"
       :desc "Kill compilation" "K" #'kill-compilation))

(map! :map (subed-mode subed-srt-mode-map)
  :n "M-p" #'subed-backward-subtitle-text
  :n "M-n" #'subed-forward-subtitle-text
  :leader                           ; Use leader key from now on
  :desc "MPV play/pause" "<RET>" #'subed-mpv-toggle-pause)

(map! :leader
      (:prefix "f"
       :desc "Find other file (.c, .h)" "o" #'ff-find-other-file))

(map! :leader
      (:prefix "n"
        (:prefix ("D" . "Denote/mynote")
         ("b" #'+mynote/browse-notes)
         ("N" #'+mynote/new-subdir)
         ("n" #'+mynote/new-in-subdir)
         ("d" #'+mynote/new-in-subdir-with-date
          (:prefix ("D" . "Denote")
            ("n" #'denote)
            ("d" #'denote-date))))))

(toggle-frame-fullscreen)

(setq confirm-kill-emacs nil)

(setq doom-modeline-major-mode-icon t)

(setq
 doom-theme 'doom-molokai
 display-line-numbers-type 'relative)

(doom-themes-treemacs-config)
(after! treemacs
  (treemacs-load-theme "doom-colors")

  (setq treemacs-follow-mode t))

(setq my-font
      (cl-find-if #'doom-font-exists-p
                    '("DejaVu Sans Code"
                      "Dejavu Sans Mono")))
(setq doom-font (font-spec :family my-font :size 15)
      doom-big-font (font-spec :family my-font :size 21))

(setq lsp-headerline-breadcrumb-enable t)

(add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook #'(lambda () (c-toggle-auto-state 1)))

(add-hook 'org-mode-hook #'+bidi-mode)

(add-hook '+bidi-mode-hook
          (lambda () (set-fontset-font t 'arabic (font-spec :family "Droid Naskh Shift Alt"))))

(after! compile
        (setq compilation-scroll-output t))

(setq org-directory "~/Documents/ORG/")
(add-hook! 'org-mode-hook 'org-download-enable)

(global-org-modern-mode)

(require 'f)
(require 'denote)
(require 'denote-dired)

(defun mynote--get-note-subdirs ()
  "Lists only names of subdirectories"
  (let ((subdir-names)
        (subdirs (f-directories denote-directory)))
    (dolist (item subdirs)
      (add-to-list 'subdir-names (file-name-nondirectory item)))
    subdir-names))

(defun mynote--set-denote-keywords ()
  "Sets `denote-keywords' based on subfolder structure"
  (setq denote-known-keywords (mynote--get-note-subdirs)))

(defun +mynote/new-subdir ()
  "Creates sub directory in the `denote-directory' for better organization"
  (interactive)
  (if-let (keyword (read-string "Subdir name: " nil))
      (let ((subdir (file-name-concat denote-directory keyword)))
        (let ((loc-file (file-name-concat subdir ".dir-locals.el")))
          (if (f-dir? subdir)
              (message (concat "directory " subdir " already exists!"))
            (progn
              (make-directory subdir)
              (if (f-file? loc-file)
                  (message (concat "file " loc-file " already exists!"))
                (progn
                  (make-empty-file loc-file)
                  (write-region "((nil . ((denote-directory . local))))" nil loc-file)))))
          (mynote--set-denote-keywords)))))

(defun +mynote/new-in-subdir ()
  "Call this function instead of `denote' for notes in a subfolder"
  (interactive)
  (let* ((keyword (denote--keywords-prompt))
         (denote-directory (file-name-concat denote-directory keyword)))
    (denote
     (denote--title-prompt)
     keyword)))

(defun +mynote/new-in-subdir-with-date ()
  "Call this function instead of `denote-date' for notes in a subfolder with date"
  (interactive)
  (let* ((keyword (denote--keywords-prompt))
         (denote-directory (file-name-concat denote-directory keyword)))
    (denote-date
     (denote--date-prompt)
     (denote--title-prompt)
     keyword)))

(defun +mynote/browse-notes ()
  "Browse files from `denote-directory'"
  (interactive)
  (unless (bound-and-true-p denote-directory)
    (message "denote-directoy not defined"))
  (doom-project-browse (concat denote-directory "/")))

(setq denote-directory "~/Documents/notes")
(mynote--set-denote-keywords)
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

(setq my-dired-ls-switches "-alh --ignore=.* --ignore=\\#* --ignore=*~")

(setq my-dired-switch 1)

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

(setq projectile-project-search-path '("~/Projects/Code"))

(add-to-list '+lookup-provider-url-alist '("Startpage" "https://www.startpage.com/do/dsearch?query=%s"))
(add-to-list '+lookup-provider-url-alist '("Qwant" "https://qwant.com/?q=%s"))

(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(setq global-undo-tree-mode t)
