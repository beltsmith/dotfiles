{ pkgs, ... }:

{
  packages = with pkgs; [
    # lutris
    steam
    # runelite

    nethack

    xboxdrv
  ];
}
