(let* ((home (getenv "HOME"))
       (local-path-list (list (concat home "/.pyenv/shims")
                              "/usr/local/bin")))
  (setenv "PATH"
          (mapconcat 'identity
                     (append local-path-list
                             (list (getenv "PATH")))
                     ":"))
  (setq exec-path (append local-path-list exec-path)))
