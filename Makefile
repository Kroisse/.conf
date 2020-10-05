TOP := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

ALACRITTY_D := $(HOME)/.config/alacritty
GITCONFIG := $(HOME)/.gitconfig
GITIGNORE := $(HOME)/.gitignore_global
VSCODE_SETTINGS := $(HOME)/Library/Application\ Support/Code/User/settings.json
ZSHRC := $(HOME)/.zshrc

install: install-alacritty install-git install-vscode install-zsh

install-alacritty:
	-rm -ri $(ALACRITTY_D)
	mkdir -p $(dir $(ALACRITTY_D))
	ln -s $(TOP)alacritty $(ALACRITTY_D)

install-git:
	-rm -i $(GITCONFIG) $(GITIGNORE)
	ln -s $(TOP)git/gitconfig $(GITCONFIG)
	ln -s $(TOP)git/gitignore $(GITIGNORE)

install-zsh: $(HOME)/.oh-my-zsh
	brew install zsh-completions
	-rm -i $(ZSHRC)
	ln -s $(TOP)zsh/zshrc $(ZSHRC)

install-vscode:
	-rm -i $(VSCODE_SETTINGS)
	ln -s $(TOP)vscode/settings.json $(VSCODE_SETTINGS)


$(HOME)/.oh-my-zsh:
	sh $(TOP)zsh/install-oh-my-zsh.sh


# for debugging

inspect:
	@echo "TOP=$(TOP)"
