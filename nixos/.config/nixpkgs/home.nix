{ pkgs, ... }:

{
  home.packages = [
    pkgs.zplug
    pkgs.glances
    pkgs.exa
  ];

  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    autocd = true;
  };

  home.file = {
    ".tmux.conf".source = ~/dotfiles/tmux/.tmux.conf;

  };
}
