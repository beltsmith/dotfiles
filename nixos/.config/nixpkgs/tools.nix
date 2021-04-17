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

    usbutils

    cifs-utils
    lsof

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

    bind # dig

    wireguard-tools
    gcc-unwrapped

    # FS

    nixfmt

    qbittorrent
    unrar

    vlc
    obs-studio
    kazam

    bc

    jetbrains.idea-community

    nix-tree
    coreutils

    grub2_full
  ];
}
