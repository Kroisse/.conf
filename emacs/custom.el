;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-no-message t)
 '(auto-save-visited-mode t)
 '(claude-code-program-switches '("--dangerously-skip-permissions"))
 '(dired-listing-switches "-alh --group-directories-first" nil nil "Customized with use-package emacs")
 '(dirvish-attributes
   '(file-time file-size subtree-state collapse git-msg vc-state))
 '(dirvish-mode-line-format '(:left (sort symlink) :right (omit index)))
 '(dirvish-override-dired-mode t)
 '(dirvish-side-follow-mode t)
 '(ivy-initial-inputs-alist
   '((counsel-package . "^+") (counsel-org-capture . "^")
     (counsel-minor . "^+") (counsel-M-x . "")
     (counsel-describe-symbol . "^") (org-refile . "^")
     (org-agenda-refile . "^") (org-capture-refile . "^")
     (Man-completion-table . "^") (woman . "^")))
 '(package-selected-packages
   '(claude-code copilot counsel dirvish eglot git-gutter gruvbox-theme
		 magit treesit-auto vterm))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el"
		  :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
	      :branch "main")))
 '(vterm-clear-scrollback-when-clearing t)
 '(vterm-min-window-width 40)
 '(warning-suppress-log-types
   '((copilot copilot-no-mode-indent) (copilot copilot-exceeds-max-char))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
