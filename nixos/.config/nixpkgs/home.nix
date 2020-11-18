{ pkgs, ... }:

{
  home.packages = [
    pkgs.glances
    pkgs.zplug
    pkgs.exa
  ];
}
