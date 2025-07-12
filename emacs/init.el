';;; -*- lexical-binding: t -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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
  :config
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
  :bind (("C-\\" . toggle-input-method)
         ("C-x O" . (lambda () (interactive) (other-window -1)))
         ("C-x k" . kill-current-buffer)))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-hard t)
  (set-face-attribute 'default nil :background "unspecified-bg")
  (when (display-graphic-p)
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t 'hangul (font-spec :family "D2Coding"))
    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)))

;; Ivy/Counsel for better file finding and completion
(use-package counsel
  :ensure t
  :bind (("C-s" . swiper-isearch)      ; Better search
	 ("C-r" . swiper-isearch-backward) ; Better backward search
         ("M-x" . counsel-M-x)         ; Better M-x
         ("C-x C-f" . counsel-find-file) ; Better find file
         ("C-c g" . counsel-git)       ; Find file in git project
         ("C-c j" . counsel-git-grep)  ; Grep in git project
         ("C-c k" . counsel-rg))       ; ag/ripgrep search
  :custom
  (ivy-use-virtual-buffers t)          ; Add recent files to switch-buffer
  (ivy-count-format "(%d/%d) ")        ; Show current/total in minibuffer)
  (ivy-height 10)                      ; Number of result lines to display
  (ivy-re-builders-alist '((t . ivy--regex-plus))) ; Space-separated words matching
  :config
  (ivy-mode 1))                        ; Enable Ivy everywhere

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

(use-package treesit
  :ensure nil
  :config
  ;; Define tree-sitter language sources
  (setq treesit-language-source-alist
        '((cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "main" "src"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))))

  ;; Auto-mode mappings
  (dolist (mode '(("/CMakeLists\\.txt\\'" . cmake-ts-mode)
                  ("\\.cmake\\'" . cmake-ts-mode)
                  ("Dockerfile\\'" . dockerfile-ts-mode)
                  ("\\.dockerfile\\'" . dockerfile-ts-mode)
                  ("\\.json\\'" . json-ts-mode)
                  ("\\.md\\'" . markdown-ts-mode)
                  ("\\.markdown\\'" . markdown-ts-mode)
                  ("\\.toml\\'" . toml-ts-mode)
                  ("\\.ya?ml\\'" . yaml-ts-mode)))
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
  (add-to-list 'project-vc-extra-root-markers ".clangd"))

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
