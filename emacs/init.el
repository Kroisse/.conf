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
  :custom
  (current-language-environment "Korean")
  (default-input-method "korean-hangul390")
  (menu-bar-mode nil)
  (vc-follow-symlinks t)
  (global-auto-revert-mode t)
  (inhibit-compacting-font-caches t)
  (which-key-mode t)
  (xterm-mouse-mode t)
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
  :config
  (defun toggle-window-dedicated ()
    "Toggle window dedication for the current window."
    (interactive)
    (let ((dedicated (window-dedicated-p)))
      (set-window-dedicated-p (selected-window) (not dedicated))
      (message "Window %s" (if dedicated "undedicated" "dedicated"))))
  :bind (("C-\\" . toggle-input-method)
	 ("C-x \\" . toggle-input-method)
         ("C-x k" . kill-current-buffer)
         ("C-c w d" . toggle-window-dedicated)
         ("C-t" . nil)))

(use-package emacs
  :ensure nil
  :config
  (load-theme 'tango t)
  (when (display-graphic-p)
    (set-face-attribute 'default nil :height 90)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)))

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
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (t . ivy--regex-plus))) ; Space-separated words matching
  (swiper-use-visual-line nil)
  (swiper-include-line-number-in-search t)
  (swiper-action-recenter t)
  (ivy-dynamic-exhibit-delay-ms 150)   ; Reduce input delay
  :config
  (setq swiper-goto-start-of-match t))

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
		 (window-width . 90)))
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
  (magit-diff-refine-hunk t))

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

(use-package desktop
  :ensure nil
  :custom
  (desktop-restore-frames t)
  :config
  (desktop-save-mode 1)
  :hook
  (server-done . desktop-save-in-desktop-dir))

(use-package tab-bar
  :ensure nil
  :after transient
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  :config
  (tab-bar-mode 1)
  
  (defun my/tab-bar-new-tab-to ()
    "Create new tab and switch to directory."
    (interactive)
    (tab-bar-new-tab)
    (call-interactively #'counsel-find-file))
  
  (defun my/tab-bar-close-other-tabs ()
    "Close all tabs except current one."
    (interactive)
    (tab-bar-close-other-tabs))
  
  (defun my/tab-bar-rename-tab-with-project ()
    "Rename tab with project name if in project."
    (interactive)
    (let ((project (project-current)))
      (if project
          (tab-bar-rename-tab (project-name project))
        (call-interactively #'tab-bar-rename-tab))))

  (transient-define-prefix my/tab-bar-menu ()
    "Tab bar management menu"
    [["Tab Navigation"
      ("n" "Next tab" tab-bar-switch-to-next-tab :transient t)
      ("p" "Previous tab" tab-bar-switch-to-prev-tab :transient t)
      ("1" "Tab 1" (lambda () (interactive) (tab-bar-select-tab 1)))
      ("2" "Tab 2" (lambda () (interactive) (tab-bar-select-tab 2)))
      ("3" "Tab 3" (lambda () (interactive) (tab-bar-select-tab 3)))
      ("4" "Tab 4" (lambda () (interactive) (tab-bar-select-tab 4)))]
     ["Tab Management"
      ("c" "New tab" tab-bar-new-tab)
      ("C" "New tab to..." my/tab-bar-new-tab-to)
      ("x" "Close tab" tab-bar-close-tab)
      ("X" "Close other tabs" my/tab-bar-close-other-tabs)
      ("r" "Rename tab" my/tab-bar-rename-tab-with-project)
      ("u" "Undo close tab" tab-bar-undo-close-tab)]
     ["Tab Actions"
      ("d" "Duplicate tab" tab-bar-duplicate-tab)
      ("g" "Switch to tab..." tab-bar-switch-to-tab)
      ("t" "Toggle tab bar" tab-bar-mode)]])
  
  :bind (("C-x t" . my/tab-bar-menu)
         ("s-1" . (lambda () (interactive) (tab-bar-select-tab 1)))
         ("s-2" . (lambda () (interactive) (tab-bar-select-tab 2)))
         ("s-3" . (lambda () (interactive) (tab-bar-select-tab 3)))
	 ("s-4" . (lambda () (interactive) (tab-bar-select-tab 4)))))

(use-package ace-window
  :ensure t
  :bind ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ; Use home row keys
  (aw-scope 'frame)                        ; Only current frame
  (aw-background t)                        ; Dim other windows
  :config
  (ace-window-display-mode 1))

; Load local configuration file if it exists
(let ((local-file (expand-file-name "init-local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;;; init.el ends here
