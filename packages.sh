#!/usr/bin/env bash
PACKAGE_FILE="$HOME/dotfiles/packages"

add_package() {
    package="$1"

    echo "$package" >>$PACKAGE_FILE
}

install_packages() {
    yay -Sy - <$PACKAGE_FILE
}
