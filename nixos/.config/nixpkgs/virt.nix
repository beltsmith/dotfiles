{ pkgs, ... }:

{
  packages = with pkgs; [

    wine

    qemu
    virt-manager

    virtualbox

    appimage-run
  ];
}
