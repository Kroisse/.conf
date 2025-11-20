;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Personal Emacs configuration

;;; Code:

;; Define color constants for consistent theming across packages
(defconst my-colors-diff-added-bg "#e8f5e8")
(defconst my-colors-diff-added-fg "#22663c")
(defconst my-colors-diff-added-highlight-bg "#d4edd4")
(defconst my-colors-diff-removed-bg "#ffeaea")
(defconst my-colors-diff-removed-fg "#cc3333")
(defconst my-colors-diff-removed-highlight-bg "#ffd6d6")

(defconst my-colors-status-good "#4e9a06")
(defconst my-colors-status-bad "#cc0000")
(defconst my-colors-status-warning "#f57900")
(defconst my-colors-status-dimmed "#888a85")

(defconst my-colors-heading "#c17d11")
(defconst my-colors-heading-secondary "#8f5902")

(defconst my-colors-branch-local "#3465a4")
(defconst my-colors-branch-upstream "#75507b")
(defconst my-colors-branch-remote "#06989a")

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package emacs
  :ensure nil
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Performance improvements
  (setq gc-cons-threshold (* 50 1000 1000)) ; 50MB during init
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold (* 8 1000 1000)))) ; 8MB after init
  :custom
  (current-language-environment "Korean")
  (default-input-method "korean-hangul390")
  (menu-bar-mode nil)
  (vc-follow-symlinks t)
  (global-auto-revert-mode t)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
  (which-key-mode t)
  (which-key-idle-delay 0.5)           ; Show which-key popup after 0.5s
  (xterm-mouse-mode t)
  (electric-pair-mode t)               ; Auto-close brackets and quotes
  (show-paren-mode t)                  ; Highlight matching parens
  (column-number-mode t)               ; Show column number in mode line
  (delete-selection-mode t)            ; Replace selected text when typing
  ;; Auto-save settings
  (auto-save-no-message t)
  (auto-save-visited-mode t)
  ;; Exit confirmation
  (confirm-kill-emacs 'y-or-n-p)
  ;; Display settings
  (recenter-redisplay nil)
  ;; Whitespace settings
  (global-whitespace-mode t)
  (whitespace-line-column 120)
  (whitespace-style '(face trailing tabs lines newline missing-newline-at-eof empty
                           indentation space-after-tab space-before-tab tab-mark
                           newline-mark))
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
  (defun my/toggle-window-dedicated ()
    "Toggle window dedication for the current window."
    (interactive)
    (let ((dedicated (window-dedicated-p)))
      (set-window-dedicated-p (selected-window) (not dedicated))
      (message "Window %s" (if dedicated "undedicated" "dedicated"))))
  (defun my/kill-this-buffer ()
    "Kill current buffer without prompt."
    (interactive)
    (kill-buffer (current-buffer)))
  :bind (("C-\\" . toggle-input-method)
         ("C-x \\" . toggle-input-method)
         ; ("C-SPC" . toggle-input-method)
         ("C-x k" . my/kill-this-buffer)
         ("C-c w d" . my/toggle-window-dedicated)
         ("C-t" . nil)))

(use-package emacs
  :ensure nil
  :config
  (set-face-attribute 'default nil :family "Noto Sans Mono" :height 80)
  (set-fontset-font t 'latin (font-spec :family "Fira Code" :weight 'normal))
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans"))
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend))

(use-package stimmung-themes
  :demand t
  :ensure t
  :custom-face
  (whitespace-indentation ((t (:background "gainsboro" :foreground "darkred"))))
  (whitespace-line ((t (:background "gainsboro" :foreground "darkred" :weight bold))))
  (whitespace-newline ((t (:foreground "gray"))))
  (whitespace-space ((t (:foreground "gray"))))
  :config (stimmung-themes-load-light))

(use-package transient :ensure nil)

;; Ivy/Counsel for better file finding and completion
(use-package counsel
  :ensure t
  :bind (("C-s" . swiper)      ; Better search
         ("C-r" . swiper-backward) ; Better backward search
         ("M-x" . counsel-M-x)         ; Better M-x
         ("C-x C-f" . counsel-find-file) ; Better find file
         ("C-c g" . counsel-git)       ; Find file in git project
         ("C-c j" . counsel-git-grep)  ; Grep in git project
         ("C-c k" . counsel-rg))       ; ag/ripgrep search
  :custom
  (ivy-mode t)
  (ivy-use-virtual-buffers t)          ; Add recent files to switch-buffer
  (ivy-count-format "(%d/%d) ")        ; Show current/total in minibuffer)
  (ivy-height 10)                      ; Number of result lines to display
  (ivy-wrap t)                         ; Wrap around at the end of candidates
  (ivy-fixed-height-minibuffer t)     ; Fixed height minibuffer
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (read-shell-command . ivy--regex)
                           (counsel-compile . regexp-quote)
                           (t . ivy--regex-plus))) ; Space-separated words matching
  (swiper-use-visual-line nil)
  (swiper-include-line-number-in-search t)
  (swiper-action-recenter t)
  (ivy-dynamic-exhibit-delay-ms 150)   ; Reduce input delay
  :config
  (setq swiper-goto-start-of-match t)
  ;; Remove initial ^ from most commands
  (setq ivy-initial-inputs-alist nil))

(use-package marginalia
  :ensure t
  :after counsel
  :custom
  (marginalia-align 'right)   ; Align annotations to the right
  (marginalia-max-relative-age 0) ; Show absolute age
  :init
  (marginalia-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(file-time file-size subtree-state collapse git-msg vc-state))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit index)))
  (dirvish-side-attributes '(vc-state))
  (dirvish-side-follow-mode t)
  (dirvish-subtree-state-style 'plus)
  :hook
  (dirvish-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :bind
  ;; Bind `dired' to `dirvish'
  (("C-x C-j" . dirvish-side)
   ("C-x d" . dirvish)))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (add-to-list 'warning-suppress-log-types '(copilot copilot-exceeds-max-char))
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)))


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc"))

(use-package treesit
  :ensure nil
  :config
  ;; Define tree-sitter language sources
  (setq treesit-language-source-alist
        '((cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

  ;; Auto-mode mappings
  (dolist (mode '(("/CMakeLists\\.txt\\'" . cmake-ts-mode)
                  ("\\.cmake\\'" . cmake-ts-mode)
                  ("Dockerfile\\'" . dockerfile-ts-mode)
                  ("\\.dockerfile\\'" . dockerfile-ts-mode)
                  ("\\.json\\'" . json-ts-mode)
                  ("\\.toml\\'" . toml-ts-mode)
                  ("\\.ya?ml\\'" . yaml-ts-mode)
                  ("\\.hpp\\'" . c++-mode)
                  ("\\.h\\'" . c++-mode)
                  ("\\.rs\\'" . rustic-mode)))
    (add-to-list 'auto-mode-alist mode)))

(use-package vterm
  :ensure t
  :custom
  (vterm-environment
   '("LANG=ko_KR.UTF-8"
     "LC_ALL=ko_KR.UTF-8"
     "LC_CTYPE=ko_KR.UTF-8"
     "TERM=xterm-256color"))
  (vterm-disable-underline t)
  (vterm-disable-inverse-video t)
  (vterm-max-scrollback 10000)
  (vterm-clear-scrollback-when-clearing t)
  (vterm-min-window-width 40)
  :config
  (setq vterm-toggle-use-dedicated-buffer t)
  (setq vterm-always-compile-module t))

(use-package project
  :ensure nil
  :after transient
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-find-dir "Find directory")
     (my/project-vterm "Terminal" ?t)
     (agent-shell-anthropic-start-claude-code "Claude Code" ?c)
     (magit-project-status "Magit" ?g)))
  :config
  (add-to-list 'project-vc-extra-root-markers ".clangd")

  ;; Add vterm to project switch commands
  (defun my/project-vterm ()
    "Start vterm in the current project's root."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-name (project-name (project-current t)))
           (vterm-buffer-name (format "*vterm-%s*" project-name)))
      (vterm)))

  (defun my/project-counsel-compile ()
    "Run counsel-compile in project root with project-compile-command support."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (compile-command (or project-compile-command compile-command)))
      (counsel-compile)))

  :bind 
  (("C-x b" . project-switch-to-buffer)
   (:map project-prefix-map
         ("c" . my/project-counsel-compile)
         ("t" . my/project-vterm))))

(use-package flycheck
  :ensure t)

(use-package agent-shell
  :ensure t
  :hook (agent-shell-mode . (lambda ()
                              (whitespace-mode -1)
                              (agent-shell-completion-mode 1)))
  :config
  (add-to-list 'display-buffer-alist
               '((derived-mode . agent-shell-mode)
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 100)))
  :bind (:map agent-shell-mode-map
              ("C-c c" . agent-shell-help-menu)))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet"
            :rev "72a18d372fef4b0971267bf13f127dcce681859a"))

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  :hook (compilation-mode . (lambda ()
                              (setq-local scroll-conservatively 10000)))
  :config
  (put 'project-compile-command 'safe-local-variable 'stringp))

(use-package eglot
  :ensure t
  :after transient
  :hook ((c-mode c++-mode) . my/eglot-ensure-with-project-root)
        (python-mode . my/eglot-ensure-with-project-root)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--enable-config"
                    "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(rustic-mode . ("rust-analyzer")))
  (defun my/eglot-ensure-with-project-root ()
    "Ensure Eglot is started with the project root."
    (when-let ((project (project-current)))
      (let ((default-directory (project-root project)))
        (eglot-ensure))))

  (transient-define-prefix my/eglot-menu ()
    "Eglot LSP management menu"
    [["Navigation"
      ("d" "Find declaration" eglot-find-declaration)
      ("D" "Find definition" xref-find-definitions)
      ("t" "Find type definition" eglot-find-typeDefinition)
      ("i" "Find implementation" eglot-find-implementation)
      ("r" "Find references" xref-find-references)
      ("b" "Go back" xref-go-back :transient t)]
     ["Code Actions"
      ("a" "Code actions" eglot-code-actions)
      ("R" "Rename" eglot-rename)
      ("f" "Format buffer" eglot-format-buffer)
      ("F" "Format region" eglot-format)
      ("o" "Organize imports" eglot-code-action-organize-imports)]
     ["Documentation"
      ; ("h" "Help at point" eglot-help-at-point)
      ("H" "Eldoc" eldoc)
      ; ("s" "Signature help" eglot-signature-help)
      ]
     ["Server Management"
      ("S" "Start/restart server" eglot-reconnect)
      ("Q" "Shutdown server" eglot-shutdown)
      ("=" "Show events" eglot-events-buffer)
      ("e" "Show stderr" eglot-stderr-buffer)
      ("q" "Quit" transient-quit-one)]])

  :bind (:map eglot-mode-map
              ("C-c l" . my/eglot-menu)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.3)              ; Faster eldoc display
  (eldoc-echo-area-use-multiline-p t) ; Allow multiline eldoc
  :config
  (global-eldoc-mode 1))

(use-package flymake
  :ensure nil
  :after transient
  :custom
  (flymake-no-changes-timeout 0.5)    ; Shorter delay for changes
  :hook ((prog-mode . flymake-mode))
  :config
  (transient-define-prefix my/flymake-menu ()
    "Flymake diagnostics management menu"
    [["Navigation"
      ("n" "Next error" flymake-goto-next-error :transient t)
      ("p" "Previous error" flymake-goto-prev-error :transient t)
      ("f" "First error" (lambda () (interactive) (goto-char (point-min)) (flymake-goto-next-error)) :transient t)
      ("l" "Last error" (lambda () (interactive) (goto-char (point-max)) (flymake-goto-prev-error)) :transient t)]
     ["Diagnostics"
      ("d" "Show buffer diagnostics" flymake-show-buffer-diagnostics)
      ("D" "Show project diagnostics" flymake-show-project-diagnostics)
      ("s" "Show diagnostic at point" flymake-show-diagnostic)
      ("c" "Check buffer" flymake-start)]
     ["Management"
      ("r" "Running backends" flymake-running-backends)
      ("R" "Disabled backends" flymake-disabled-backends)
      ("b" "Reporting backends" flymake-reporting-backends)
      ("m" "Mode line" flymake-mode)
      ("q" "Quit" transient-quit-one)]])

  :bind (:map flymake-mode-map
              ("C-c !" . my/flymake-menu)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*magit"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 15)))
  :custom-face
  (magit-bisect-bad ((t (:inherit my-status-bad-base))))
  (magit-bisect-good ((t (:inherit my-status-good-base))))
  (magit-bisect-skip ((t (:inherit my-status-warning-base))))
  (magit-blame-date ((t (:inherit my-status-dimmed-base))))
  (magit-blame-heading ((t (:background "#d3d7cf" :foreground "#5f615c"))))
  (magit-blame-highlight ((t (:background "#babdb6" :foreground "#2e3436"))))
  (magit-blame-name ((t (:inherit my-branch-upstream-base))))
  (magit-branch-current ((t (:foreground "#204a87" :weight bold :box t))))
  (magit-branch-local ((t (:inherit my-branch-local-base))))
  (magit-branch-remote ((t (:inherit my-status-good-base))))
  (magit-branch-upstream ((t (:inherit my-branch-upstream-base))))
  (magit-branch-warning ((t (:inherit my-status-warning-base))))
  (magit-diff-added ((t (:background ,my-colors-diff-added-bg :foreground ,my-colors-diff-added-fg))))
  (magit-diff-added-highlight ((t (:background ,my-colors-diff-added-highlight-bg :foreground ,my-colors-diff-added-fg))))
  (magit-diff-context ((t (:foreground "#5f615c"))))
  (magit-diff-context-highlight ((t (:background "#d3d7cf" :foreground "#2e3436"))))
  (magit-diff-file-heading ((t (:weight bold))))
  (magit-diff-file-heading-highlight ((t (:background "#babdb6" :weight bold))))
  (magit-diff-file-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902"))))
  (magit-diff-hunk-heading ((t (:background "#d3d7cf" :foreground "#5f615c"))))
  (magit-diff-hunk-heading-highlight ((t (:background "#babdb6" :foreground "#2e3436"))))
  (magit-diff-hunk-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902"))))
  (magit-diff-lines-boundary ((t (:background "#f57900" :foreground "#eeeeec"))))
  (magit-diff-lines-heading ((t (:background "#f57900" :foreground "#eeeeec"))))
  (magit-diff-removed ((t (:background ,my-colors-diff-removed-bg :foreground ,my-colors-diff-removed-fg))))
  (magit-diff-removed-highlight ((t (:background ,my-colors-diff-removed-highlight-bg :foreground ,my-colors-diff-removed-fg))))
  (magit-dimmed ((t (:inherit my-status-dimmed-base))))
  (magit-hash ((t (:inherit my-status-dimmed-base))))
  (magit-header-line ((t (:inherit nil :weight bold))))
  (magit-header-line-log-select ((t (:inherit nil :weight bold))))
  (magit-log-author ((t (:foreground "#ce5c00"))))
  (magit-log-date ((t (:inherit my-status-dimmed-base))))
  (magit-log-graph ((t (:foreground "#5f615c"))))
  (magit-process-ng ((t (:inherit my-status-bad-base :weight bold))))
  (magit-process-ok ((t (:inherit my-status-good-base :weight bold))))
  (magit-reflog-amend ((t (:inherit my-branch-upstream-base))))
  (magit-reflog-checkout ((t (:inherit my-branch-local-base))))
  (magit-reflog-cherry-pick ((t (:inherit my-status-good-base))))
  (magit-reflog-commit ((t (:inherit my-status-good-base))))
  (magit-reflog-merge ((t (:inherit my-status-good-base))))
  (magit-reflog-other ((t (:inherit my-branch-remote-base))))
  (magit-reflog-rebase ((t (:inherit my-branch-upstream-base))))
  (magit-reflog-remote ((t (:inherit my-branch-remote-base))))
  (magit-reflog-reset ((t (:inherit my-status-bad-base))))
  (magit-section-heading ((t (:inherit my-heading-base))))
  (magit-section-highlight ((t (:background "#eeeeec"))))
  (magit-section-secondary-heading ((t (:inherit my-heading-secondary-base))))
  (magit-sequence-done ((t (:inherit my-status-dimmed-base))))
  (magit-sequence-drop ((t (:inherit my-status-bad-base))))
  (magit-sequence-onto ((t (:inherit my-status-dimmed-base))))
  (magit-sequence-pick ((t (:inherit my-heading-base))))
  (magit-tag ((t (:foreground "#c4a000"))))
  (transient-argument ((t (:inherit my-status-good-base))))
  (transient-disabled-suffix ((t (:inherit my-status-dimmed-base))))
  (transient-enabled-suffix ((t (:inherit my-status-good-base :weight bold))))
  (transient-heading ((t (:inherit my-heading-base))))
  (transient-inactive-argument ((t (:inherit my-status-dimmed-base))))
  (transient-inactive-value ((t (:inherit my-status-dimmed-base))))
  (transient-key ((t (:foreground "#204a87" :weight bold))))
  (transient-mismatched-key ((t (:inherit my-status-bad-base :underline t))))
  (transient-nonstandard-key ((t (:inherit my-status-warning-base :underline t))))
  (transient-separator ((t (:foreground "#d3d7cf"))))
  (transient-unreachable ((t (:foreground "#babdb6"))))
  (transient-unreachable-key ((t (:foreground "#babdb6"))))
  (transient-value ((t (:inherit my-branch-upstream-base)))))

(use-package smerge-mode
  :ensure nil
  :custom-face
  (smerge-mine ((t (:background ,my-colors-diff-removed-bg :foreground ,my-colors-diff-removed-fg))))
  (smerge-other ((t (:background ,my-colors-diff-added-bg :foreground ,my-colors-diff-added-fg))))
  (smerge-base ((t (:background "#f5f5dc" :foreground "#8b7355"))))
  (smerge-markers ((t (:background "#e6e6e6" :foreground "#666666"))))
  (smerge-refined-added ((t (:background ,my-colors-diff-added-highlight-bg :foreground ,my-colors-diff-added-fg))))
  (smerge-refined-removed ((t (:background ,my-colors-diff-removed-highlight-bg :foreground ,my-colors-diff-removed-fg))))
  (smerge-refined-changed ((t (:background "#fff2cc" :foreground "#cc6600"))))
  (smerge-upper ((t (:inherit smerge-mine))))
  (smerge-lower ((t (:inherit smerge-other)))))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (global-git-gutter-mode 1))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode 1))

(use-package imenu-list
  :ensure t
  :bind ("C-c i" . imenu-list-smart-toggle)
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-auto-delay 0.1)         ; Delay before showing completion
  (corfu-auto-prefix 1)          ; Minimum prefix length
  (corfu-quit-no-match t)        ; Quit if no match
  (corfu-preview-current nil)    ; Disable current candidate preview
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :ensure t
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; Corfu extensions (built-in with corfu from GNU ELPA)
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle))
  :config
  (corfu-popupinfo-mode 1))

(use-package corfu-info
  :ensure nil
  :after corfu
  :bind (:map corfu-map
              ("M-h" . corfu-info-documentation)
              ("M-g" . corfu-info-location)))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; Use home row keys
  (aw-scope 'frame)                        ; Only current frame
  (aw-background t)                        ; Dim other windows
  :config
  (ace-window-display-mode 1))

(if nil
(use-package perspective
  :ensure t
  :after (counsel project)
  :bind
  (("C-x C-b" . persp-list-buffers)
   ("C-x b" . persp-switch-to-buffer*)
   ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-interactive-completion-function #'ivy-completing-read)
  (persp-state-default-file (expand-file-name "persp-state" user-emacs-directory))
  (persp-show-modestring 'header)
  ;; Share perspectives across frames
  (persp-purge-initial-persp-on-save nil)
  (persp-sort 'created)
  :hook
  ((kill-emacs . #'persp-state-save)
   (project-switch-project . (lambda ()
                               (persp-switch (project-name (project-current))))))
  :config
  ;; Enable perspective-mode after frame is created (daemon-safe)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (unless (bound-and-true-p persp-mode)
                      (persp-mode 1)
                      ;; Load saved state if it exists
                      (when (file-exists-p persp-state-default-file)
                        (persp-state-load persp-state-default-file))))))
    (persp-mode 1)
    ;; Load saved state on initial startup
    (when (file-exists-p persp-state-default-file)
      (persp-state-load persp-state-default-file))))
)

(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . my/eglot-ensure-with-project-root)
  :custom
  (rustic-format-on-save t)
  (rustic-lsp-client 'eglot)
  :config
  (setq rustic-format-trigger 'on-save)
  (put 'rustic-indent-offset 'safe-local-variable 'integerp))

(use-package editorconfig
  :ensure nil
  :custom
  (editorconfig-mode t))

; Load local configuration file if it exists
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;;; init.el ends here
