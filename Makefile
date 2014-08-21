install: install-git install-emacs

install-git:
	rm -f ~/.gitconfig ~/.gitignore_global
	ln -s `pwd`/git/gitconfig ~/.gitconfig
	ln -s `pwd`/git/gitignore ~/.gitignore_global

install-emacs:
	rm -rf ~/.emacs ~/.emacs.d/site-lisp
	ln -s `pwd`/emacs/emacs.el ~/.emacs
	mkdir -p ~/.emacs.d
	ln -s `pwd`/emacs/site-lisp ~/.emacs.d/site-lisp
	cp `pwd`/emacs/emacs-local-sample.el ~/.emacs_local
