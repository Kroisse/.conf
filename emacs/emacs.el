;; -*- coding: utf-8 -*-

(when (>= emacs-major-version 24)
  (require 'package)
  ;; add Marmalade package repo.
  ;; http://marmalade-repo.org/
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

(defconst site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/"))

(defun walk-subdirs (dir action)
  (dolist (path (directory-files dir t "^[^\.]"))
    (when (file-directory-p path)
      (funcall action (file-name-as-directory path)))))

(add-to-list 'load-path site-lisp-dir)
(walk-subdirs site-lisp-dir (lambda (path) (add-to-list 'load-path path)))

(defun frame-size-according-to-size (width height)
  (let ((width (cond ((>= width 1280) 165)
                           ((>= width 1024) 120)
                           (else 80))))
    (list (cons 'width (if (> (frame-char-width)
                              (+ (/ (frame-char-height) 2) 2))
                           (/ width 2)
                           width))
          (cons 'height (/ (- (x-display-pixel-height) 140)
                           (frame-char-height))))))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (when window-system
    (let ((size (frame-size-according-to-size (x-display-pixel-width)
					      (x-display-pixel-height))))
      (dolist (elem size)
        (add-to-list 'default-frame-alist elem)))))

(defun set-buffer-process-coding-system-utf-8-unix ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;;;; emacs-customize settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-fuzzy-cursor-color "")
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(desktop-path (quote ("~")))
 '(desktop-save-mode t)
 '(fci-rule-color "#eee8d5")
 '(fill-column 80)
 '(fringe-mode (quote (0)) nil (fringe))
 '(global-auto-complete-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(google-this-mode t)
 '(help-at-pt-display-when-idle (quote (keymap local-map button kbd-help flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.8)
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice "~/workspace")
 '(js-indent-level 4)
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(js2-include-gears-externs nil)
 '(js2-include-rhino-externs nil)
 '(linum-format "%3d ")
 '(mouse-wheel-progressive-speed nil)
 '(mumamo-chunk-coloring 2)
 '(mumamo-set-major-mode-delay 0.1)
 '(ns-command-modifier (quote meta))
 '(ns-right-command-modifier (quote super))
 '(overflow-newline-into-fringe t)
 '(python-check-command "flake8")
 '(rst-level-face-base-color "#888")
 '(rst-level-face-base-light 85)
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(scss-compile-at-save nil)
 '(server-mode t)
 '(show-paren-mode t)
 '(term-exec-hook (quote (set-buffer-process-coding-system-utf-8-unix)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(vc-annotate-background "#586e75")
 '(vc-annotate-color-map (quote ((20 . "#990A1B") (40 . "#FF6E64") (60 . "#cb4b16") (80 . "#7B6000") (100 . "#b58900") (120 . "#DEB542") (140 . "#546E00") (160 . "#859900") (180 . "#B4C342") (200 . "#3F4D91") (220 . "#6c71c4") (240 . "#9EA0E5") (260 . "#2aa198") (280 . "#69CABF") (300 . "#00629D") (320 . "#268bd2") (340 . "#69B7F0") (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C")
 '(x-select-enable-clipboard t)
 '(zencoding-indentation 2))

(setq system-uses-terminfo nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-infoline ((t (:underline "green"))) t)
 '(flymake-warnline ((t (:underline "yellow")))))

;;;; user-defined settings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c $") 'multi-term)
(global-unset-key (kbd "S-SPC"))

(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))

(add-hook 'term-mode-hook
          (lambda ()
            (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

(show-paren-mode t)

(add-hook 'css-mode-hook 'css-color-mode)
(autoload 'css-mode "css-mode" "" t)
(autoload 'css-color-mode "mon-css-color""" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
(add-to-list 'auto-mode-alist '("templates/.*\\.html\\'" . jinja-mode))
(add-to-list 'auto-mode-alist '("\\.rstx\\'" . rst-mode))

(set-frame-size-according-to-resolution)
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(let ((alpha '(90 67)))
  (set-frame-parameter (selected-frame) 'alpha alpha)
  (add-to-list 'default-frame-alist (cons 'alpha alpha)))
(split-window-horizontally)
;(add-hook 'after-init-hook
;	  (lambda () (when (>= (frame-width) 168) (split-window-horizontally))))

(let ((local-conf (concat (getenv "HOME") "/.emacs-local")))
  (when (file-readable-p local-conf)
    (load-file local-conf)))

(provide 'emacs)
;;; emacs.el ends here
