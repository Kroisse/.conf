;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-no-message t)
 '(auto-save-visited-mode t)
 '(compilation-scroll-output 'first-error)
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
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"
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
 
 ;; Magit faces customized for Tango Light theme
 '(magit-branch-current ((t (:foreground "#204a87" :weight bold :box t)))) ; blue-3
 '(magit-branch-local ((t (:foreground "#3465a4")))) ; blue-2
 '(magit-branch-remote ((t (:foreground "#4e9a06")))) ; cham-3
 '(magit-branch-upstream ((t (:foreground "#75507b")))) ; plum-2
 '(magit-branch-warning ((t (:foreground "#f57900")))) ; orange-2
 
 '(magit-hash ((t (:foreground "#888a85")))) ; alum-4
 '(magit-tag ((t (:foreground "#c4a000")))) ; butter-3
 
 '(magit-section-heading ((t (:foreground "#c17d11" :weight bold)))) ; choc-2
 '(magit-section-highlight ((t (:background "#eeeeec")))) ; alum-1
 '(magit-section-secondary-heading ((t (:foreground "#8f5902" :weight bold)))) ; choc-3
 
 '(magit-diff-added ((t (:background "#8ae234" :foreground "#346604")))) ; cham-1 bg, cham-4 fg
 '(magit-diff-added-highlight ((t (:background "#73d216" :foreground "#346604")))) ; cham-2 bg, cham-4 fg
 '(magit-diff-removed ((t (:background "#ef2929" :foreground "#a40000")))) ; red-1 bg, red-3 fg
 '(magit-diff-removed-highlight ((t (:background "#cc0000" :foreground "#eeeeec")))) ; red-2 bg, alum-1 fg
 '(magit-diff-context ((t (:foreground "#5f615c")))) ; alum-5
 '(magit-diff-context-highlight ((t (:background "#d3d7cf" :foreground "#2e3436")))) ; alum-2 bg, alum-6 fg
 
 '(magit-diff-file-heading ((t (:weight bold))))
 '(magit-diff-file-heading-highlight ((t (:background "#babdb6" :weight bold)))) ; alum-3
 '(magit-diff-file-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902")))) ; orange-1 bg, choc-3 fg
 
 '(magit-diff-hunk-heading ((t (:background "#d3d7cf" :foreground "#5f615c")))) ; alum-2 bg, alum-5 fg
 '(magit-diff-hunk-heading-highlight ((t (:background "#babdb6" :foreground "#2e3436")))) ; alum-3 bg, alum-6 fg
 '(magit-diff-hunk-heading-selection ((t (:background "#fcaf3e" :foreground "#8f5902")))) ; orange-1 bg, choc-3 fg
 
 '(magit-diff-lines-boundary ((t (:background "#f57900" :foreground "#eeeeec")))) ; orange-2 bg, alum-1 fg
 '(magit-diff-lines-heading ((t (:background "#f57900" :foreground "#eeeeec")))) ; orange-2 bg, alum-1 fg
 
 '(magit-blame-heading ((t (:background "#d3d7cf" :foreground "#5f615c")))) ; alum-2 bg, alum-5 fg
 '(magit-blame-highlight ((t (:background "#babdb6" :foreground "#2e3436")))) ; alum-3 bg, alum-6 fg
 '(magit-blame-date ((t (:foreground "#888a85")))) ; alum-4
 '(magit-blame-name ((t (:foreground "#75507b")))) ; plum-2
 
 '(magit-log-author ((t (:foreground "#ce5c00")))) ; orange-3
 '(magit-log-date ((t (:foreground "#888a85")))) ; alum-4
 '(magit-log-graph ((t (:foreground "#5f615c")))) ; alum-5
 
 '(magit-process-ok ((t (:foreground "#4e9a06" :weight bold)))) ; cham-3
 '(magit-process-ng ((t (:foreground "#cc0000" :weight bold)))) ; red-2
 
 '(magit-reflog-amend ((t (:foreground "#75507b")))) ; plum-2
 '(magit-reflog-checkout ((t (:foreground "#3465a4")))) ; blue-2
 '(magit-reflog-cherry-pick ((t (:foreground "#4e9a06")))) ; cham-3
 '(magit-reflog-commit ((t (:foreground "#4e9a06")))) ; cham-3
 '(magit-reflog-merge ((t (:foreground "#4e9a06")))) ; cham-3
 '(magit-reflog-other ((t (:foreground "#06989a")))) ; cyan-2
 '(magit-reflog-rebase ((t (:foreground "#75507b")))) ; plum-2
 '(magit-reflog-remote ((t (:foreground "#06989a")))) ; cyan-2
 '(magit-reflog-reset ((t (:foreground "#cc0000")))) ; red-2
 
 '(magit-bisect-bad ((t (:foreground "#cc0000")))) ; red-2
 '(magit-bisect-good ((t (:foreground "#4e9a06")))) ; cham-3
 '(magit-bisect-skip ((t (:foreground "#f57900")))) ; orange-2
 
 '(magit-sequence-pick ((t (:foreground "#c17d11")))) ; choc-2
 '(magit-sequence-drop ((t (:foreground "#cc0000")))) ; red-2
 '(magit-sequence-done ((t (:foreground "#888a85")))) ; alum-4
 '(magit-sequence-onto ((t (:foreground "#888a85")))) ; alum-4
 
 '(magit-dimmed ((t (:foreground "#888a85")))) ; alum-4
 '(magit-header-line ((t (:inherit nil :weight bold))))
 '(magit-header-line-log-select ((t (:inherit nil :weight bold))))
 )

;;; custom.el ends here
