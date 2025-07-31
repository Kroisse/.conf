;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Personal Emacs configuration

;;; Code:

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
  (inhibit-compacting-font-caches t)
  (which-key-mode t)
  (which-key-idle-delay 0.5)           ; Show which-key popup after 0.5s
  (xterm-mouse-mode t)
  (electric-pair-mode t)               ; Auto-close brackets and quotes
  (show-paren-mode t)                  ; Highlight matching parens
  (column-number-mode t)               ; Show column number in mode line
  (delete-selection-mode t)            ; Replace selected text when typing
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
  :bind (("C-\\" . toggle-input-method)
	 ("C-x \\" . toggle-input-method)
	 ("C-SPC" . toggle-input-method)
         ("C-c w d" . my/toggle-window-dedicated)
         ("C-t" . nil)))

(use-package emacs
  :ensure nil
  :config
  (load-theme 'tango t)
  (set-face-attribute 'default nil :family "Noto Sans Mono" :height 80)
  (set-fontset-font t 'latin (font-spec :family "Fira Code" :weight 'normal))
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans"))
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend))

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
                  ("\\.h\\'" . c++-mode)))
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
     (project-vterm "Terminal" ?t)
     (claude-code "Claude Code" ?c)
     (magit-project-status "Magit" ?g)))
  :config
  (add-to-list 'project-vc-extra-root-markers ".clangd")
  
  ;; Add vterm to project switch commands
  (defun project-vterm ()
    "Start vterm in the current project's root."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-name (project-name (project-current t)))
           (vterm-buffer-name (format "*vterm-%s*" project-name)))
      (vterm)))

  (transient-define-prefix my/project-menu ()
    "Project management menu"
    [["Find & Search"
      ("f" "Find file" project-find-file)
      ("d" "Find directory" project-find-dir)
      ("r" "Find regexp" project-find-regexp)
      ("R" "Query replace" project-query-replace-regexp)]
     ["Buffers & Windows"
      ("b" "Switch to buffer" project-switch-to-buffer)
      ("B" "List buffers" project-list-buffers)
      ("k" "Kill buffers" project-kill-buffers)
      ("D" "Dired" project-dired)]
     ["Build & Run"
      ("c" "Compile" project-compile)
      ("C" "Configure compile" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'project-compile))))
      ("v" "Vterm" project-vterm)]
     ["Project Management"
      ("p" "Switch project" project-switch-project)
      ("P" "Forget zombie projects" project-forget-zombie-projects)
      ("!" "Execute shell command" project-shell-command)
      ("&" "Async shell command" project-async-shell-command)
      ("g" "Magit status" magit-project-status)]])
  
  :bind (:map project-prefix-map
              ("m" . my/project-menu)))

(use-package flycheck
  :ensure t)

(use-package claude-code
  :ensure nil
  :after (vterm transient)
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
	    :rev :newest
	    :branch "main")
  :custom
  (claude-code-terminal-backend 'vterm)
  (claude-code-vterm-buffer-multiline-output t)
  :config
  (claude-code-mode)
  (add-to-list 'display-buffer-alist
	       '("^\\*claude"
		 (display-buffer-in-side-window)
		 (side . right)
		 (window-width . 100)))
  :bind-keymap ("C-c c" . claude-code-command-map))

(use-package compile
  :ensure nil
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
      ("b" "Go back" xref-go-back)]
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
      ("e" "Show stderr" eglot-stderr-buffer)]])
  
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
      ("m" "Mode line" flymake-mode)]])
  
  :bind (:map flymake-mode-map
	      ("C-c !" . my/flymake-menu)))
  
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  :custom-face
  (magit-bisect-bad ((t (:foreground "#cc0000"))))
  (magit-bisect-good ((t (:foreground "#4e9a06"))))
  (magit-bisect-skip ((t (:foreground "#f57900"))))
  (magit-blame-date ((t (:foreground "#888a85"))))
  (magit-blame-heading ((t (:background "#d3d7cf" :foreground "#5f615c"))))
  (magit-blame-highlight ((t (:background "#babdb6" :foreground "#2e3436"))))
  (magit-blame-name ((t (:foreground "#75507b"))))
  (magit-branch-current ((t (:foreground "#204a87" :weight bold :box t))))
  (magit-branch-local ((t (:foreground "#3465a4"))))
  (magit-branch-remote ((t (:foreground "#4e9a06"))))
  (magit-branch-upstream ((t (:foreground "#75507b"))))
  (magit-branch-warning ((t (:foreground "#f57900"))))
  (magit-diff-added ((t (:background "#e8f5e8" :foreground "#22663c"))))
  (magit-diff-added-highlight ((t (:background "#d4edd4" :foreground "#22663c"))))
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
  (magit-diff-removed ((t (:background "#ffeaea" :foreground "#cc3333"))))
  (magit-diff-removed-highlight ((t (:background "#ffd6d6" :foreground "#cc3333"))))
  (magit-dimmed ((t (:foreground "#888a85"))))
  (magit-hash ((t (:foreground "#888a85"))))
  (magit-header-line ((t (:inherit nil :weight bold))))
  (magit-header-line-log-select ((t (:inherit nil :weight bold))))
  (magit-log-author ((t (:foreground "#ce5c00"))))
  (magit-log-date ((t (:foreground "#888a85"))))
  (magit-log-graph ((t (:foreground "#5f615c"))))
  (magit-process-ng ((t (:foreground "#cc0000" :weight bold))))
  (magit-process-ok ((t (:foreground "#4e9a06" :weight bold))))
  (magit-reflog-amend ((t (:foreground "#75507b"))))
  (magit-reflog-checkout ((t (:foreground "#3465a4"))))
  (magit-reflog-cherry-pick ((t (:foreground "#4e9a06"))))
  (magit-reflog-commit ((t (:foreground "#4e9a06"))))
  (magit-reflog-merge ((t (:foreground "#4e9a06"))))
  (magit-reflog-other ((t (:foreground "#06989a"))))
  (magit-reflog-rebase ((t (:foreground "#75507b"))))
  (magit-reflog-remote ((t (:foreground "#06989a"))))
  (magit-reflog-reset ((t (:foreground "#cc0000"))))
  (magit-section-heading ((t (:foreground "#c17d11" :weight bold))))
  (magit-section-highlight ((t (:background "#eeeeec"))))
  (magit-section-secondary-heading ((t (:foreground "#8f5902" :weight bold))))
  (magit-sequence-done ((t (:foreground "#888a85"))))
  (magit-sequence-drop ((t (:foreground "#cc0000"))))
  (magit-sequence-onto ((t (:foreground "#888a85"))))
  (magit-sequence-pick ((t (:foreground "#c17d11"))))
  (magit-tag ((t (:foreground "#c4a000"))))
  (transient-argument ((t (:foreground "#4e9a06"))))
  (transient-disabled-suffix ((t (:foreground "#888a85"))))
  (transient-enabled-suffix ((t (:foreground "#4e9a06" :weight bold))))
  (transient-heading ((t (:foreground "#c17d11" :weight bold))))
  (transient-inactive-argument ((t (:foreground "#888a85"))))
  (transient-inactive-value ((t (:foreground "#888a85"))))
  (transient-key ((t (:foreground "#204a87" :weight bold))))
  (transient-mismatched-key ((t (:foreground "#cc0000" :underline t))))
  (transient-nonstandard-key ((t (:foreground "#f57900" :underline t))))
  (transient-separator ((t (:foreground "#d3d7cf"))))
  (transient-unreachable ((t (:foreground "#babdb6"))))
  (transient-unreachable-key ((t (:foreground "#babdb6"))))
  (transient-value ((t (:foreground "#75507b")))))

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
  :hook
  ((kill-emacs . #'persp-state-save)
   (project-switch-project . (lambda ()
			       (persp-switch (project-name (project-current))))))
  :init
  (persp-mode))

(use-package editorconfig
  :ensure nil
  :custom
  (editorconfig-mode t))

; Load local configuration file if it exists
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;;; init.el ends here
