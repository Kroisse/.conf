;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-no-message t)
 '(auto-save-visited-mode t)
 '(claude-code-mode t)
 '(claude-code-program-switches '("--dangerously-skip-permissions"))
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'y-or-n-p)
 '(default-input-method "korean-hangul390" nil nil "Customized with use-package emacs")
 '(dired-listing-switches "-alh --group-directories-first" nil nil "Customized with use-package emacs")
 '(dirvish-attributes
   '(file-time file-size subtree-state collapse git-msg vc-state))
 '(dirvish-mode-line-format '(:left (sort symlink) :right (omit index)))
 '(dirvish-override-dired-mode t)
 '(dirvish-side-attributes '(vc-state))
 '(dirvish-side-follow-mode t)
 '(dirvish-subtree-state-style 'plus)
 '(ivy-initial-inputs-alist
   '((counsel-package . "^+") (counsel-org-capture . "^")
     (counsel-minor . "^+") (counsel-M-x . "")
     (counsel-describe-symbol . "^") (org-refile . "^")
     (org-agenda-refile . "^") (org-capture-refile . "^")
     (Man-completion-table . "^") (woman . "^")))
 '(package-selected-packages
   '(ace-window breadcrumb claude-code copilot corfu-terminal counsel
		dirvish eglot flycheck git-gutter gruvbox-theme
		imenu-list magit markdown-mode vterm))
 '(package-vc-selected-packages
   '((claude-code :url
		  "https://github.com/stevemolitor/claude-code.el"
		  :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
	      :branch "main")))
 '(recenter-redisplay nil)
 '(vterm-clear-scrollback-when-clearing t)
 '(vterm-min-window-width 40)
 '(warning-suppress-log-types
   '((copilot copilot-no-mode-indent) (copilot copilot-exceeds-max-char))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-bisect-bad ((t (:foreground "#cc0000"))))
 '(magit-bisect-good ((t (:foreground "#4e9a06"))))
 '(magit-bisect-skip ((t (:foreground "#f57900"))))
 '(magit-blame-date ((t (:foreground "#888a85"))))
 '(magit-blame-heading ((t (:background "#d3d7cf" :foreground "#5f615c"))))
 '(magit-blame-highlight ((t (:background "#babdb6" :foreground "#2e3436"))))
 '(magit-blame-name ((t (:foreground "#75507b"))))
 '(magit-branch-current ((t (:foreground "#204a87" :weight bold :box t))))
 '(magit-branch-local ((t (:foreground "#3465a4"))))
 '(magit-branch-remote ((t (:foreground "#4e9a06"))))
 '(magit-branch-upstream ((t (:foreground "#75507b"))))
 '(magit-branch-warning ((t (:foreground "#f57900"))))
 '(magit-diff-added ((t (:background "#8ae234" :foreground "#346604"))))
 '(magit-diff-added-highlight ((t (:background "#73d216" :foreground "#346604"))))
 '(magit-diff-context ((t (:foreground "#5f615c"))))
 '(magit-diff-context-highlight ((t (:background "#d3d7cf" :foreground "#2e3436"))))
 '(magit-diff-file-heading ((t (:weight bold))))
 '(magit-diff-file-heading-highlight ((t (:background "#babdb6" :weight bold))))
 '(magit-diff-file-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902"))))
 '(magit-diff-hunk-heading ((t (:background "#d3d7cf" :foreground "#5f615c"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#babdb6" :foreground "#2e3436"))))
 '(magit-diff-hunk-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902"))))
 '(magit-diff-lines-boundary ((t (:background "#f57900" :foreground "#eeeeec"))))
 '(magit-diff-lines-heading ((t (:background "#f57900" :foreground "#eeeeec"))))
 '(magit-diff-removed ((t (:background "#ef2929" :foreground "#a40000"))))
 '(magit-diff-removed-highlight ((t (:background "#cc0000" :foreground "#eeeeec"))))
 '(magit-dimmed ((t (:foreground "#888a85"))))
 '(magit-hash ((t (:foreground "#888a85"))))
 '(magit-header-line ((t (:inherit nil :weight bold))))
 '(magit-header-line-log-select ((t (:inherit nil :weight bold))))
 '(magit-log-author ((t (:foreground "#ce5c00"))))
 '(magit-log-date ((t (:foreground "#888a85"))))
 '(magit-log-graph ((t (:foreground "#5f615c"))))
 '(magit-process-ng ((t (:foreground "#cc0000" :weight bold))))
 '(magit-process-ok ((t (:foreground "#4e9a06" :weight bold))))
 '(magit-reflog-amend ((t (:foreground "#75507b"))))
 '(magit-reflog-checkout ((t (:foreground "#3465a4"))))
 '(magit-reflog-cherry-pick ((t (:foreground "#4e9a06"))))
 '(magit-reflog-commit ((t (:foreground "#4e9a06"))))
 '(magit-reflog-merge ((t (:foreground "#4e9a06"))))
 '(magit-reflog-other ((t (:foreground "#06989a"))))
 '(magit-reflog-rebase ((t (:foreground "#75507b"))))
 '(magit-reflog-remote ((t (:foreground "#06989a"))))
 '(magit-reflog-reset ((t (:foreground "#cc0000"))))
 '(magit-section-heading ((t (:foreground "#c17d11" :weight bold))))
 '(magit-section-highlight ((t (:background "#eeeeec"))))
 '(magit-section-secondary-heading ((t (:foreground "#8f5902" :weight bold))))
 '(magit-sequence-done ((t (:foreground "#888a85"))))
 '(magit-sequence-drop ((t (:foreground "#cc0000"))))
 '(magit-sequence-onto ((t (:foreground "#888a85"))))
 '(magit-sequence-pick ((t (:foreground "#c17d11"))))
 '(magit-tag ((t (:foreground "#c4a000"))))
 '(transient-argument ((t (:foreground "#4e9a06"))))
 '(transient-disabled-suffix ((t (:foreground "#888a85"))))
 '(transient-enabled-suffix ((t (:foreground "#4e9a06" :weight bold))))
 '(transient-heading ((t (:foreground "#c17d11" :weight bold))))
 '(transient-inactive-argument ((t (:foreground "#888a85"))))
 '(transient-inactive-value ((t (:foreground "#888a85"))))
 '(transient-key ((t (:foreground "#204a87" :weight bold))))
 '(transient-mismatched-key ((t (:foreground "#cc0000" :underline t))))
 '(transient-nonstandard-key ((t (:foreground "#f57900" :underline t))))
 '(transient-separator ((t (:foreground "#d3d7cf"))))
 '(transient-unreachable ((t (:foreground "#babdb6"))))
 '(transient-unreachable-key ((t (:foreground "#babdb6"))))
 '(transient-value ((t (:foreground "#75507b")))))

;;; custom.el ends here
