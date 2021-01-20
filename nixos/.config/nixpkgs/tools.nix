{ pkgs, ... }:

{
  packages = with pkgs; [
    zplug
    glances
    exa
    gitAndTools.hub
    barrier
    fasd
    fzf
    direnv
    nix-direnv
    jq
    tree

    nfs-utils
    unzip

    gparted
    winusb

    pass
    awscli2
    aws-vault

    rofi

    gnumake

    arandr
    feh

    poppler_utils

    ispell

    zip

    time

    valgrind
  ];
}
