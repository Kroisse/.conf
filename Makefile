install: install-git install-emacs

install-git:
	rm -f ~/.gitconfig
	ln -s `pwd`/git/gitconfig ~/.gitconfig

install-emacs:
	rm -rf ~/.emacs ~/.emacs.d/site-lisp
	ln -s `pwd`/emacs/emacs ~/.emacs
	mkdir -p ~/.emacs.d
	ln -s `pwd`/emacs/site-lisp ~/.emacs.d/site-lisp
