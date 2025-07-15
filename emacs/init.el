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
         ("C-x O" . (lambda () (interactive) (other-window -1)))
         ("C-x k" . kill-current-buffer)
         ("C-c w d" . toggle-window-dedicated)
         ("C-t" . nil)))

(use-package emacs
  :ensure nil
  :config
  (load-theme 'tango t)
  (set-face-attribute 'default nil :background "unspecified-bg" :foreground "black")
  (when (display-graphic-p)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)))

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
  (ivy-mode 1)
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

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

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
  (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)
              (setq-local line-spacing 0)
)))
  ;; Quick launcher for Claude Code
  ;; (defun claude-code (&optional yolo)
  ;;   "Open Claude Code in vterm. With prefix arg, skip permissions."
  ;;   (interactive "P")
  ;;   (let ((default-directory (or (vc-root-dir) default-directory)))
  ;;     (vterm (if yolo "*claude-code-yolo*" "*claude-code*"))
  ;;     (vterm-send-string
  ;;      (if yolo
  ;; 	   "claude --dangerously-skip-permissions\n"
  ;; 	 "claude\n"))))
  ;; (defun my/send-korean-to-vterm (text)
  ;;   "한글 텍스트를 vterm에 전송"
  ;;   (interactive "s한글 입력: ")
  ;;   (vterm-send-string text))
  ;; :bind (("C-c c" . claude-code)))

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el"
	    :rev :newest
	    :branch "main")
  :config
  (setq claude-code-terminal-backend 'vterm)
  (setq claude-code-vterm-buffer-multiline-output t)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

(use-package project
  :ensure nil
  :config
  (add-to-list 'project-vc-extra-root-markers ".clangd")
  
  ;; Add vterm to project switch commands
  (defun project-vterm ()
    "Start vterm in the current project's root."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-name (project-name (project-current t)))
           (vterm-buffer-name (format "*vterm-%s*" project-name)))
      (vterm))))

(use-package compile
  :ensure nil
  :hook (compilation-mode . (lambda ()
                              (setq-local scroll-conservatively 10000)))
  :config
  (put 'project-compile-command 'safe-local-variable 'stringp))

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
	      ("C-c l r" . eglot-rename)
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l f" . eglot-format)
	      ("C-c l d" . eglot-find-declaration)
	      ("C-c l t" . eglot-find-typeDefinition)
	      ("C-c l h" . eglot-help-at-point))
  :hook ((c-mode c++-mode) . my/eglot-ensure-with-project-root)
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
  (defun my/eglot-ensure-with-project-root ()
    "Ensure Eglot is started with the project root."
    (when-let ((project (project-current)))
      (let ((default-directory (project-root project)))
	(eglot-ensure)))))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
	      ("C-c ! l" . flymake-show-buffer-diagnostics) ; Show diagnostics
	      ("C-c ! n" . flymake-goto-next-error) ; Go to next error
	      ("C-c ! p" . flymake-goto-prev-error)) ; Go to previous error
  :custom
  (flymake-no-changes-timeout 0.5)    ; Shorter delay for changes
  :hook ((prog-mode . flymake-mode)))
  
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch)
	 ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  :custom-face
  (magit-popup-key ((t (:foreground "#0969da" :weight bold))))
  (magit-popup-heading ((t (:foreground "#24292f" :weight bold))))
  (magit-popup-option-value ((t (:foreground "#cf222e"))))
  (magit-section-highlight ((t (:background "#f6f8fa"))))
  (magit-diff-added ((t (:background "#d1f4d0" :foreground "#116329"))))
  (magit-diff-removed ((t (:background "#ffeef0" :foreground "#82071e"))))
  (magit-diff-added-highlight ((t (:background "#a7f3d0" :foreground "#116329"))))
  (magit-diff-removed-highlight ((t (:background "#fecdd3" :foreground "#82071e"))))
  (transient-key ((t (:foreground "#0969da" :weight bold))))
  (transient-heading ((t (:foreground "#24292f" :weight bold))))
  (transient-value ((t (:foreground "#cf222e"))))
  (transient-argument ((t (:foreground "#8250df"))))
  (transient-unreachable ((t (:foreground "#6e7781"))))
  (transient-inapt-suffix ((t (:foreground "#6e7781"))))
  (transient-inactive-argument ((t (:foreground "#6e7781"))))
  (transient-inactive-value ((t (:foreground "#6e7781")))))

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
      ("1" "Tab 1" (lambda () (interactive) (tab-bar-select-tab 1)) :transient t)
      ("2" "Tab 2" (lambda () (interactive) (tab-bar-select-tab 2)) :transient t)
      ("3" "Tab 3" (lambda () (interactive) (tab-bar-select-tab 3)) :transient t)
      ("4" "Tab 4" (lambda () (interactive) (tab-bar-select-tab 4)) :transient t)]
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

;;; init.el ends here
