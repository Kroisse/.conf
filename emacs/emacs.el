;; -*- coding: utf-8 -*-

(when (>= emacs-major-version 24)
  (require 'package)
  ;; add Marmalade package repo.
  ;; http://marmalade-repo.org/
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

(defconst site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/"))

(defun walk-subdirs (dir action)
  (dolist (path (directory-files dir t "^[^\.]"))
    (when (file-directory-p path)
      (funcall action (file-name-as-directory path)))))

(add-to-list 'load-path site-lisp-dir)
(walk-subdirs site-lisp-dir (lambda (path) (add-to-list 'load-path path)))

(setenv "PATH"
        (concat "/usr/local/bin" ":"
                (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))

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
 '(column-number-mode t)
 '(css-indent-offset 4)
 '(desktop-path (quote ("~")))
 '(desktop-save-mode t)
 '(fill-column 80)
 '(fringe-mode (quote (0)) nil (fringe))
 '(help-at-pt-display-when-idle (quote (keymap local-map button kbd-help flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.8)
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
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t)
 '(nxml-child-indent 4)
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
 '(x-select-enable-clipboard t)
 '(zencoding-indentation 2))

(color-theme-solarized-light)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-infoline ((t (:underline "green"))))
 '(flymake-warnline ((t (:underline "yellow")))))

;;;; user-defined settings

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c $") 'multi-term)
(global-unset-key (kbd "S-SPC"))

(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))

(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

(show-paren-mode t)

(when (>= emacs-major-version 23)
  (load (concat (file-name-as-directory site-lisp-dir) "nxhtml/autostart"))
  (require 'jinja))

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


;; flymake using python-check-command
(when (load "flymake" t)
 (defun flymake-pyflakes-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name 
                       temp-file 
                       (file-name-directory buffer-file-name)))
          (py-check-cmd (split-string-and-unquote python-check-command))
          (py-check-exe (car py-check-cmd))
          (py-check-arg (cdr py-check-cmd)))
     (list py-check-exe (append py-check-arg
                                (list local-file)))))
 (add-to-list 'flymake-allowed-file-name-masks 
              '("\\.py\\'" flymake-pyflakes-init))
 (add-hook 'python-mode-hook 'flymake-find-file-hook)

 (defun flymake-rust-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
     (list "/usr/local/bin/rustc" (list "--no-trans" local-file))))
 (add-to-list 'flymake-allowed-file-name-masks
              '(".+\\.r[cs]$" flymake-rust-init
                flymake-simple-cleanup flymake-get-real-file-name)))