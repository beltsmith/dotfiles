{ pkgs, ... }:

{
  home.packages = [
    pkgs.zplug
    pkgs.glances
    pkgs.exa
    pkgs.fasd
  ];

  programs.home-manager.enable = true;

  programs.zsh = {
    enable = true;
    autocd = true;
    initExtra = ''
      eval "$(hub alias -s)"
      eval "$(fasd --oinit auto)"
    '';
    shellAliases = ''
      l="exa -lgh"
      la='l -a'
      lm="l -smodified"
    '';
  };

  home.file = {
    ".tmux.conf".source = ~/dotfiles/tmux/.tmux.conf;
  };
}
