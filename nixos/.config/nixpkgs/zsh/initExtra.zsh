append_path() {
    export PATH="$PATH:$1"
}

prepend_path() {
    export PATH="$1:$PATH"
}
prepend_path "$HOME/.cargo/bin"
export GOPATH="$HOME/go"
append_path "$GOPATH/bin"
append_path "$HOME/bin"
append_path "$HOME/scripts"
append_path "$HOME/games"
append_path "/usr/local/bin"
append_path "$HOME/.local/bin"
append_path "$HOME/.emacs.d/bin"

export AWS_VAULT_BACKEND=pass

eval "$(hub alias -s)"
eval "$(fasd --oinit auto)"

autoload -Uz compinit
compinit

# bindkey -e
bindkey -v
bindkey "\e." insert-last-word
bindkey -M viins '\e.' insert-last-word

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

bindkey "\e[3~" delete-char

setopt autopushd

bindkey '^r' history-incremental-search-backward
# function zle-line-init zle-keymap-select {
#     VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]% %{$reset_color%}"
#     RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
#     zle reset-prompt
# }
# zle -N zle-line-init
# zle -N zle-keymap-select

export KEYTIMEOUT=10
