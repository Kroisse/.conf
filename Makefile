TOP := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_SETTINGS := $(HOME)/.emacs
EMACS_D := $(HOME)/.emacs.d
EMACS_SITE_LISP := $(EMACS_D)/site-lisp
GITCONFIG := $(HOME)/.gitconfig
GITIGNORE := $(HOME)/.gitignore_global
ZSHRC := $(HOME)/.zshrc

install: install-git install-emacs install-zsh

install-git:
	-rm -i $(GITCONFIG) $(GITIGNORE)
	ln -s $(TOP)git/gitconfig $(GITCONFIG)
	ln -s $(TOP)git/gitignore $(GITIGNORE)

install-emacs:
	-rm -ri $(EMACS_SETTINGS) $(EMACS_SITE_LISP)
	ln -s $(TOP)emacs/emacs.el $(EMACS_SETTINGS)
	mkdir -p $(EMACS_D)
	ln -s $(TOP)emacs/site-lisp $(EMACS_SITE_LISP)
	cp $(TOP)emacs/emacs-local-sample.el $(EMACS_SETTINGS)_local

install-zsh: $(HOME)/.oh-my-zsh
	-rm -i $(ZSHRC)
	ln -s $(TOP)zsh/zshrc $(ZSHRC)

$(HOME)/.oh-my-zsh:
	sh $(TOP)zsh/install-oh-my-zsh.sh


# for debugging

inspect:
	@echo "TOP=$(TOP)"
	@echo "EMACS_SITE_LISP=$(EMACS_SITE_LISP)"
