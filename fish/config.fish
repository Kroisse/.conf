set SCRIPT_PATH (dirname (status -f))

if not set -q fish_local_config
    set -g fish_local_config "$HOME/.config/fish/config.local.fish"
end

set -g EDITOR 'emacsclient --alternate-editor=vim'


### oh-my-fish configs

# Path to your oh-my-fish.
set fish_path $SCRIPT_PATH/oh-my-fish

# Theme
set fish_theme agnoster-powerline

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-fish/plugins/*)
# Custom plugins may be added to ~/.oh-my-fish/custom/plugins/
# Example format: set fish_plugins autojump bundler
set fish_plugins brew autoenv

# Path to your custom folder (default path is $FISH/custom)
set fish_custom $SCRIPT_PATH/custom

# Load oh-my-fish cofiguration.
. $fish_path/oh-my-fish.fish


### load local configurations

if [ -f $fish_local_config ]
    . $fish_local_config
end
