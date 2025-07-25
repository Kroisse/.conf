TOP := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

ALACRITTY_D := $(HOME)/.config/alacritty
CLAUDE_D := $(HOME)/.claude
EMACS_D := $(HOME)/.emacs.d
EMACS_INIT := $(HOME)/.emacs.d/init.el
EMACS_CUSTOM := $(HOME)/.emacs.d/custom.el
GITCONFIG := $(HOME)/.gitconfig
GITIGNORE := $(HOME)/.gitignore_global
TMUX_CONF := $(HOME)/.tmux.conf
VSCODE_SETTINGS := $(HOME)/Library/Application\ Support/Code/User/settings.json
ZSHRC := $(HOME)/.zshrc

install: install-alacritty install-claude install-emacs install-git install-tmux install-vscode install-zsh

install-alacritty:
	-rm -ri $(ALACRITTY_D)
	mkdir -p $(dir $(ALACRITTY_D))
	ln -s $(TOP)alacritty $(ALACRITTY_D)

install-claude:
	mkdir -p $(CLAUDE_D)
	-rm -ri $(CLAUDE_D)/agents $(CLAUDE_D)/hooks $(CLAUDE_D)/commands
	ln -s $(TOP)claude/agents $(CLAUDE_D)/agents
	ln -s $(TOP)claude/hooks $(CLAUDE_D)/hooks
	ln -s $(TOP)claude/commands $(CLAUDE_D)/commands

install-emacs:
	mkdir -p $(EMACS_D)
	-rm -i $(EMACS_INIT) $(EMACS_CUSTOM)
	ln -s $(TOP)emacs/init.el $(EMACS_INIT)
	ln -s $(TOP)emacs/custom.el $(EMACS_CUSTOM)

install-git:
	-rm -i $(GITCONFIG) $(GITIGNORE)
	ln -s $(TOP)git/gitconfig $(GITCONFIG)
	ln -s $(TOP)git/gitignore $(GITIGNORE)

install-tmux:
	-rm -i $(TMUX_CONF)
	ln -s $(TOP)tmux/tmux.conf $(TMUX_CONF)

install-vscode:
	-rm -i $(VSCODE_SETTINGS)
	ln -s $(TOP)vscode/settings.json $(VSCODE_SETTINGS)

install-zsh: $(HOME)/.oh-my-zsh
	@if command -v brew >/dev/null 2>&1; then \
		brew install zsh-completions; \
	else \
		echo "Homebrew not found, skipping zsh-completions install."; \
	fi
	-rm -i $(ZSHRC)
	ln -s $(TOP)zsh/zshrc $(ZSHRC)

$(HOME)/.oh-my-zsh:
	sh $(TOP)zsh/install-oh-my-zsh.sh


# for debugging

inspect:
	@echo "TOP=$(TOP)"
