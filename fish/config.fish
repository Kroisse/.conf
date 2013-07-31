set SCRIPT_PATH (dirname (status -f))

if not set -q FISH_LOCAL_CONFIG
    set -g FISH_LOCAL_CONFIG "$HOME/.config/fish/config.local.fish"
end

set -g EDITOR 'emacsclient --alternate-editor=vim'

if [ -f "$SCRIPT_PATH/autoenv/autoenv.fish" ]
    . "$SCRIPT_PATH/autoenv/autoenv.fish"
end

if [ -f $FISH_LOCAL_CONFIG ]
    . $FISH_LOCAL_CONFIG
end
