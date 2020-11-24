append_path() {
    export PATH="$PATH:$1"
}

prepend_path() {
    export PATH="$1:$PATH"
}
prepend_path "$HOME/.rbenv/bin"
prepend_path "$HOME/.cargo/bin"
export GOPATH="$HOME/go"
append_path "$GOPATH/bin"
append_path "$HOME/bin"
append_path "$HOME/scripts"
append_path "/usr/local/bin"
append_path "$HOME/.local/bin"

eval "$(hub alias -s)"
eval "$(fasd --oinit auto)"
