{ pkgs, ... }:

{
  packages = with pkgs; [

    wine

    qemu
    virt-manager

    appimage-run
  ];
}
