#!/usr/bin/bash

declare -a messaging=("slack-desktop" "discord") # zulip

declare -a emacs_packages=("emacs-git" "ispell")

declare -a editors=("${emacs_packages[@]}" "vim" "vscode" "intellij-idea-community-edition")

#

# gems rufo solargraph

declare -a ruby_packages=("ruby")

# ocamlFormat
declare -a langs=("${ruby_packages[@]}"
                  "rust"
                  "cargo"
                  "editorconfig-core-c"
                  "plantuml"
                  "afew"
                  "ocaml"
                  "dune")

#

declare -a zsh_tools=("zsh" "zplug")

declare -a shell_tools=("fasd" "fzf" "direnv" "exa" "jq" "tree" "asdf-vm")

declare -a shells=("${zsh_tools[@]}" "elvish")

declare -a shell_packages=("${shells[@]}" "${shell_tools[@]}")

#

declare -a git_tools=("git" "github-cli")

declare -a system_tools=("${git_tools[@]}" "aws-cli" "aws-vault" "etcd" "docker" "docker-compose" "kubectl" "heroku")

declare -a services=("redis" "postgresql" "nginx")

#

declare -a music=("spotify" "cmus")

declare -a bluetooth_packages=("bluez" "bluez-utils" "bluez-tools" "blueman")

declare -a tools=("${bluetooth_packages[@]}" "${system_tools[@]}")

#

declare -a virt=("virtualbox" "wine" "qemu" "libappimage")

#

declare -a fonts=("nerd-fonts-complete")

declare -a terminal_tools=("tmux" "glances" "up" "fd")

declare -a terminals=("alacritty" "kitty" "${terminal_tools}")

declare -a browsers=("chromium" "google-chrome" "google-chrome-dev")

declare -a graphics=("zathura" "xournal")

declare -a desktop=(\
    "${fonts[@]}"
    "${messaging[@]}"
    "${terminals[@]}"
    "${browsers[@]}"
    "${graphics[@]}"
    "zoom"
    "pinentry"
    "pinentry-rofi"
    "feh" "synergy" "polybar" "rofi"
    "pavucontrol"
    "dunst"
    "lastpass-cli"
    "dmenu-lpass-nu"
    "xsel"
    "flameshot" "picom" "redshift" "pasystray" "udiskie" "nitrogen")

#

declare -a games=("steam" "lutris" "runelite" "nethack")

#

declare -a media=("ffmpeg" "handbrake")

#

declare -a unsorted_packages=("gparted" "unzip" "lsof" "pass" "zip"
                              "bind" "obs-studio" "bc")

#

declare -a packages=(\
    "${editors[@]}"
    "${langs[@]}"
    "${games[@]}"
    "${media[@]}"
    "${desktop[@]}"
    "${shell_packages[@]}"
    "${music[@]}"
    "${virt[@]}"
    "${tools[@]}"
    "${services[@]}"
    "${unsorted_packages[@]}")

not_installed() {
    local package="$@"
    # return ! yay -Q $package &> /dev/null
    yay -Q $package &> /dev/null
    if [ $? -ne 0 ]; then
        return 0
    else
        return 1
    fi
    # return $? && 0 || 1
    # if $?; then retu
}

install_packages() {
    local packages=("$@")

    for package in "${packages[@]}"
    do
        if not_installed "$package"; then
            echo "Installing: $package"
            yay -S $package
        else
            echo "Skipping: $package"
        fi
    done
}

yay -Syy

install_packages "${packages[@]}"

yay -Su
