;;; -*- lexical-binding: t -*-

;; Optimize garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Restore after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

;; Disable UI elements early
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Prevent flash of unstyled modeline
; (setq-default mode-line-format nil)

;; Don't resize the frame
(setq frame-inhibit-implied-resize t)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t))
