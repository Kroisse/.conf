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
 '(global-whitespace-mode t)
 '(ivy-initial-inputs-alist
   '((counsel-package . "^+") (counsel-org-capture . "^")
     (counsel-minor . "^+") (counsel-M-x . "")
     (counsel-describe-symbol . "^") (org-refile . "^")
     (org-agenda-refile . "^") (org-capture-refile . "^")
     (Man-completion-table . "^") (woman . "^")))
 '(package-selected-packages
   '(ace-window breadcrumb claude-code copilot corfu-terminal counsel
		dirvish eglot flycheck git-gutter gruvbox-theme
		helpful imenu-list magit marginalia markdown-mode
		monet perspective rustic vterm))
 '(package-vc-selected-packages
   '((monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url
		  "https://github.com/stevemolitor/claude-code.el"
		  :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
	      :branch "main")))
 '(recenter-redisplay nil)
 '(safe-local-variable-values '((rustic-indent-offset . 2)))
 '(vterm-clear-scrollback-when-clearing t)
 '(vterm-min-window-width 40)
 '(warning-suppress-log-types
   '((copilot copilot-no-mode-indent) (copilot copilot-exceeds-max-char)))
 '(whitespace-line-column 120)
 '(whitespace-style
   '(face trailing tabs lines newline missing-newline-at-eof empty
	  indentation space-after-tab space-before-tab tab-mark
	  newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
